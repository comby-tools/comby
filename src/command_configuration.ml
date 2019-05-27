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
  type t =
    | Match_printer of (string option -> Match.t list -> unit)
    | Rewrite_printer of (string option -> Rewrite.match_context_replacement list -> string -> string -> unit)

  module Match : sig
    val create : output_options -> t
  end = struct
    let create output_options =
      let pp source_path matches =
        let ppf = Format.std_formatter in
        match output_options with
        | { json_pretty = true; json_lines = true; _ }
        | { json_pretty = true; json_lines = false; _ } ->
          Format.fprintf ppf "%a" Match.pp_json_pretty (source_path, matches)
        | { json_pretty = false; json_lines = true; _ } ->
          Format.fprintf ppf "%a" Match.pp_json_lines (source_path, matches)
        | _ ->
          Format.fprintf ppf "%a" Match.pp_match_result (source_path, matches)
      in
      Match_printer pp
  end

  module Rewrite : sig
    val create : output_options -> t
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

    let create (output_options : output_options) : t =
      let ppf = Format.std_formatter in
      let pp path replacements result source_content =
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
      in
      Rewrite_printer pp
  end
end

type t =
  { sources : Command_input.t
  ; specifications : Specification.t list
  ; file_extensions : string list option
  ; run_options : run_options
  ; output_printer : Printer.t
  }

let create
    { input_options =
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
    }
  : t Or_error.t =
  let () =
    match Rule.create rule with
    | Ok _ -> ()
    | Error error ->
      let message = Error.to_string_hum error in
      Format.printf "Match rule parse error: %s@." message;
      exit 1
  in
  let specifications =
    match specification_directories, anonymous_arguments with
    | None, None
    | Some [], None ->
      Format.eprintf
        "Please specify templates. Either on the command line, or using \
         -templates [dir]@.";
      exit 1
    | None, Some (match_template, rewrite_template, _) ->
      if match_only then
        [Specification.create ~match_template ~match_rule:rule ()]
      else
        [Specification.create ~match_template ~rewrite_template ~match_rule:rule ~rewrite_rule:rule ()]
    | Some specification_directories, None ->
      parse_specification_directories match_only specification_directories
    | Some specification_directories, Some _ ->
      Format.eprintf
        "Warning: ignoring match and rewrite templates and rules on \
         commandline and using those in directories instead@.";
      parse_specification_directories match_only specification_directories
  in
  let stdin, file_extensions =
    match anonymous_arguments with
    | Some (_, _, None) -> true, file_extensions
    | Some (_, _, Some file_extensions) -> false, (Some (List.concat file_extensions))
    (* No anonymous arguments: if -stdin was specified, this lets
       -templates work with stdin. *)
    | None -> stdin, file_extensions
  in
  if stdin && (Option.is_some zip_file) then
    (Format.eprintf "-zip may not be used with stdin";
     exit 1)
  else if stdin && in_place then
    (Format.eprintf "-i may not be used with stdin";
     exit 1);
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
  let output_printer =
    if match_only then
      Printer.Match.create output_options
    else
      Printer.Rewrite.create output_options
  in
  Ok { sources
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
