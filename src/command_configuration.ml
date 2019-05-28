open Core

open Language
open Rewriter

let read = Fn.compose String.rstrip In_channel.read_all

let read_template =
  Fn.compose
    String.chop_suffix_exn ~suffix:"\n"
    In_channel.read_all

(** If users give e.g., *.c, convert it to .c *)
let fake_glob_file_extensions file_extensions =
  List.map file_extensions ~f:(String.substr_replace_all ~pattern:"*" ~with_:"")

let parse_specification_directories match_only specification_directory_paths =
  let parse_directory path =
    let match_template =
      let filename = path ^/ "match" in
      try read_template filename
      with _ -> failwith (Format.sprintf "Could not read required match file %s" filename)
    in
    let match_rule =
      let filename = path ^/ "match_rule" in
      try Some (read filename)
      with _ -> None
    in
    let rewrite_template =
      let filename = path ^/ "rewrite" in
      if match_only then
        None
      else
        try Some (read_template filename)
        with _ -> None
    in
    let rewrite_rule =
      let filename = path ^/ "rewrite_rule" in
      if match_only then
        None
      else
        try Some (read filename)
        with _ -> None
    in
    Specification.create ~match_template ?match_rule ?rewrite_template ?rewrite_rule ()
  in
  List.map specification_directory_paths ~f:parse_directory

let parse_source_directories ?(file_extensions = []) target_directory =
  let rec ls_rec path =
    if Sys.is_file path = `Yes then
      match file_extensions with
      | [] -> [path]
      | suffixes when List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix path) ->
        [path]
      | _ -> []
    else
      try
        Sys.ls_dir path
        |> List.map ~f:(fun sub -> ls_rec (Filename.concat path sub))
        |> List.concat
      with
      | _ -> []
  in
  ls_rec target_directory

type output_options =
  { json_pretty : bool
  ; json_lines : bool
  ; in_place : bool
  ; stdin : bool
  ; output_diff : bool
  }

type user_input_options =
  { rule : string
  ; specification_directories : string list option
  ; anonymous_arguments : (string * string * string list list option) option
  ; file_extensions : string list option
  ; zip_file : string option
  ; match_only : bool
  ; target_directory : string
  }

type run_options =
  { sequential : bool
  ; verbose : bool
  ; match_timeout : int
  ; number_of_workers : int
  ; dump_statistics : bool
  }

type user_input =
  { input_options : user_input_options
  ; run_options : run_options
  ; output_options : output_options
  }

module Printer = struct
  type printable_result =
    | Matches of
        { source_path : string option
        ; matches : Match.t list
        }
    | Replacements of
        { source_path : string option
        ; replacements : Rewrite.match_context_replacement list
        ; result : string
        ; source_content : string
        }

  type t = printable_result -> unit

  module Match : sig

    val print : output_options -> string option -> Match.t list -> unit

  end = struct

    let print output_options source_path matches =
      let ppf = Format.std_formatter in
      match output_options with
      | { json_pretty = true; json_lines = true; _ }
      | { json_pretty = true; json_lines = false; _ } ->
        Format.fprintf ppf "%a" Match.pp_json_pretty (source_path, matches)
      | { json_pretty = false; json_lines = true; _ } ->
        Format.fprintf ppf "%a" Match.pp_json_lines (source_path, matches)
      | _ ->
        Format.fprintf ppf "%a" Match.pp_match_result (source_path, matches)

  end

  module Rewrite : sig

    val print : output_options -> string option -> Rewrite.match_context_replacement list -> string -> string -> unit

  end = struct

    let get_diff path source_content result =
      let open Patdiff_lib in
      let configuration = Diff_configuration.plain () in
      let prev = Patdiff_core.{ name = path; text = source_content } in
      let next = Patdiff_core.{ name = path; text = result } in
      Compare_core.diff_strings
        ~print_global_header:true
        configuration
        ~prev
        ~next
      |> function
      | `Different diff -> Some diff
      | `Same -> None

    (* only used in rewrite *)
    let get_json_rewrites replacements result =
      let value = `List (List.map ~f:Rewrite.match_context_replacement_to_yojson replacements) in
      `Assoc [("uri", `Null); ("rewritten_source", `String result); ("in_place_substitutions", value)]

    (* only used in rewrite *)
    let json_rewrites replacements (path: string) (diff: string) result =
      let value =
        `List (List.map ~f:Rewrite.match_context_replacement_to_yojson replacements) in
      `Assoc
        [ ("uri", `String path)
        ; ("rewritten_source", `String result)
        ; ("in_place_substitutions", value)
        ; ("diff", `String diff)
        ]

    let print output_options path replacements result source_content =
      let ppf = Format.std_formatter in
      match path, output_options with
      (* rewrite in place *)
      | Some path, { json_pretty = false; json_lines = false; stdin = false; in_place = true; _ } ->
        Out_channel.write_all path ~data:result
      (* stdin, not JSON *)
      | _, { json_pretty = false; json_lines = false; stdin = false; in_place = false; _ } ->
        Format.fprintf ppf "%s%!" result
      (* JSON with path *)
      | Some path, { json_pretty = true; in_place = false; _ } ->
        let diff = get_diff path source_content result in
        Option.value_map diff ~default:() ~f:(fun diff ->
            Format.printf "%s%!" @@ Yojson.Safe.pretty_to_string @@ json_rewrites replacements path diff result)
      | Some path, { json_lines = true; in_place = false; _ } ->
        let diff = get_diff path source_content result in
        Option.value_map diff ~default:() ~f:(fun diff ->
            Format.printf "%s@." @@ Yojson.Safe.to_string @@ json_rewrites replacements path diff result)
      (* stdin, JSON, no path *)
      | None, { json_pretty = true; in_place = false; _ } ->
        Format.printf "%s%!" @@ Yojson.Safe.pretty_to_string @@ get_json_rewrites replacements result
      | None, { json_lines = true; in_place = false; _ } ->
        Format.printf "%s@." @@ Yojson.Safe.to_string @@ get_json_rewrites replacements result
      (* stdout for everything else *)
      | in_, { output_diff = true; _ } ->
        let diff = get_diff (Option.value_exn in_) source_content result in
        Option.value_map diff ~default:() ~f:(fun diff -> Format.printf "%s@." diff)
      | None, _ ->
        (* if on stdin, print out, even if it's the same file *)
        Format.printf "%s%!" result
      | _ ->
        (* if it's not on stdin, we already handled the path rewrite case, so just do nothing and
           ignore the result *)
        ()
  end
end

type t =
  { sources : Command_input.t
  ; specifications : Specification.t list
  ; file_extensions : string list option
  ; run_options : run_options
  ; output_printer : Printer.t
  }

let validate_errors
    { input_options =
        { rule
        ; specification_directories
        ; anonymous_arguments
        ; zip_file
        ; _
        }
    ; run_options = _
    ; output_options =
        {
          in_place;
          stdin;
          _
        }
    } =
  let violations =
    [ stdin && Option.is_some zip_file
    , "-zip may not be used with stdin."
    ; stdin && in_place
    , "-i may not be used with stdin."
    ; anonymous_arguments = None &&
      (specification_directories = None
       || specification_directories = Some []),
      "No templates specified. \
       Either on the command line, or \
       using -templates \
       <directory-containing-templates>"
    ; let result = Rule.create rule in
      Or_error.is_error result,
      if Or_error.is_error result then
        Format.sprintf "Match rule parse error: %s@." @@
        Error.to_string_hum (Option.value_exn (Result.error result))
      else
        "UNREACHABLE"
      ;
    ]
  in
  List.filter_map violations ~f:(function
      | true, message -> Some (Or_error.error_string message)
      | _ -> None)
  |> Or_error.combine_errors_unit
  |> Result.map_error ~f:(fun error ->
      let message =
        let rec to_string acc =
          function
          | Sexp.Atom s -> s
          | List [] -> ""
          | List (x::[]) -> to_string acc x
          | List (x::xs) ->
            (List.fold xs ~init:acc ~f:to_string) ^ "\nNext error: " ^ to_string acc x
        in
        Error.to_string_hum error
        |> Sexp.of_string
        |> to_string ""
      in
      Error.of_string message)

let emit_warnings { input_options; output_options; _ } =
  let warn_on =
    [ is_some input_options.specification_directories
      && is_some input_options.anonymous_arguments,
      "Templates specified on the command line AND using -templates. Ignoring match
      and rewrite templates on the command line and only using those in directories."
    ; output_options.json_lines = true && output_options.json_pretty = true,
      "Both -json-lines and -json-pretty specified. Using -json-pretty."
    ]
  in
  List.iter warn_on ~f:(function
      | true, message -> Format.eprintf "Warning: %s@." message
      | _ -> ());
  Ok ()

let create
    ({ input_options =
         { rule
         ; specification_directories
         ; anonymous_arguments
         ; file_extensions
         ; zip_file
         ; match_only
         ; target_directory
         }
     ; run_options =
         { sequential
         ; verbose
         ; match_timeout
         ; number_of_workers
         ; dump_statistics
         }
     ; output_options =
         ({
           in_place;
           stdin;
           _
         } as output_options)
     } as configuration)
  : t Or_error.t =
  let open Or_error in
  validate_errors configuration >>= fun () ->
  emit_warnings configuration >>= fun () ->
  let specifications =
    match specification_directories, anonymous_arguments with
    | None, Some (match_template, rewrite_template, _) ->
      if match_only then
        [Specification.create ~match_template ~match_rule:rule ()]
      else
        [Specification.create ~match_template ~rewrite_template ~match_rule:rule ~rewrite_rule:rule ()]
    | Some specification_directories, _ ->
      parse_specification_directories match_only specification_directories
    | _ -> assert false
  in
  let stdin, file_extensions =
    (* Really activate stdin mode if not in the 3rd anonymous arg?
       Is the 3rd arnonymous arg meant to case out on a matcher kind, filter, or
       control stdin activation? *)
    match anonymous_arguments with
    | Some (_, _, None) -> true, file_extensions
    | Some (_, _, Some file_extensions) -> false, (Some (List.concat file_extensions))
    (* No anonymous arguments: if -stdin was specified, this lets
       -templates work with stdin. *)
    | None -> stdin, file_extensions
  in
  let sources =
    match stdin, zip_file with
    | true, _ ->
      `String (In_channel.input_all In_channel.stdin)
    | _, Some zip_file ->
      `Zip zip_file
    (* Recurse in directories *)
    | false, None ->
      let file_extensions = Option.map file_extensions ~f:fake_glob_file_extensions in
      `Paths (parse_source_directories ?file_extensions target_directory)
  in
  let in_place = if is_some zip_file then false else in_place in
  let output_options = { output_options with in_place } in

  let output_printer printable =
    let open Printer in
    match printable with
    | Matches { source_path; matches } ->
      Printer.Match.print output_options source_path matches
    | Replacements { source_path; replacements; result; source_content } ->
      Printer.Rewrite.print output_options source_path replacements result source_content
  in
  return
    { sources
    ; specifications
    ; file_extensions
    ; run_options =
        { sequential
        ; verbose
        ; match_timeout
        ; number_of_workers
        ; dump_statistics
        }
    ; output_printer
    }
