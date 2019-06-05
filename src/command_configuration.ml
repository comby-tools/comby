open Core

open Language

let read filename =
  In_channel.read_all filename
  |> fun template ->
  String.chop_suffix template ~suffix:"\n"
  |> Option.value ~default:template

let parse_specification_directories match_only specification_directory_paths =
  let parse_directory path =
    let match_template =
      let filename = path ^/ "match" in
      match read filename with
      | content -> content
      | exception _ ->
        Format.eprintf "Could not read required match file %s@." filename;
        exit 1
    in
    let read_optional filename =
      match read filename with
      | content -> Some content
      | exception _ -> None
    in
    let match_rule = read_optional (path ^/ "match_rule") in
    let rewrite_template = read_optional (path ^/ "rewrite") in
    let rewrite_rule = if match_only then None else read_optional (path ^/ "rewrite_rule") in
    Specification.create ~match_template ?match_rule ?rewrite_template ?rewrite_rule ()
  in
  List.map specification_directory_paths ~f:parse_directory

let parse_source_directories ?(file_extensions = []) target_directory =
  let rec ls_rec path =
    if Sys.is_file path = `Yes then
      match file_extensions with
      | [] -> [path]
      | suffixes when List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix path) -> [path]
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
  ; diff : bool
  ; stdout : bool
  }

type anonymous_arguments =
  { match_template : string
  ; rewrite_template : string
  ; extensions : string list option
  }

type user_input_options =
  { rule : string
  ; stdin : bool
  ; specification_directories : string list option
  ; anonymous_arguments : anonymous_arguments option
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

type input_source =
  | Stdin
  | Zip
  | Directory

module Printer = struct

  type printable_result =
    | Matches of
        { source_path : string option
        ; matches : Match.t list
        }
    | Replacements of
        { source_path : string option
        ; replacements : Replacement.t list
        ; result : string
        ; source_content : string
        }

  type t = printable_result -> unit

  module Match : sig

    type match_output =
      | Json_lines
      | Json_pretty
      | Diff
      | Number_of_matches

    val convert : output_options -> match_output

    val print : match_output -> string option -> Match.t list -> unit

  end = struct

    type match_output =
      | Json_lines
      | Json_pretty
      | Diff
      | Number_of_matches

    let convert output_options =
      match output_options with
      | { json_pretty = true; json_lines = true; _ }
      | { json_pretty = true; json_lines = false; _ } -> Json_pretty
      | { json_pretty = false; json_lines = true; _ } -> Json_lines
      | _ -> Number_of_matches

    let print (match_output : match_output) source_path matches =
      let ppf = Format.std_formatter in
      match match_output with
      | Json_lines -> Format.fprintf ppf "%a" Match.pp_json_lines (source_path, matches)
      | Json_pretty -> Format.fprintf ppf "%a" Match.pp_json_pretty (source_path, matches)
      | Number_of_matches -> Format.fprintf ppf "%a" Match.pp_match_result (source_path, matches)
      | Diff -> assert false
  end

  module Rewrite : sig

    type replacement_output =
      | In_place
      | Stdout
      | Json_lines
      | Json_pretty
      | Diff

    val convert : output_options -> replacement_output


    val print : replacement_output -> string option -> Replacement.t list -> string -> string -> unit

  end = struct

    type replacement_output =
      | In_place
      | Stdout
      | Json_lines
      | Json_pretty
      | Diff

    let convert output_options : replacement_output =
      match output_options with
      | { json_pretty = false; json_lines = false; stdout = false; in_place = true; _ } -> In_place
      | { json_pretty = true; in_place = false; _ } -> Json_pretty
      | { json_lines = true; in_place = false; _ } -> Json_lines
      | { diff = true; _ } -> Diff
      | { stdout = true; _ } -> Stdout
      | _ -> Diff

    let print replacement_output path replacements replacement_content source_content =
      let ppf = Format.std_formatter in
      match path, replacement_output with
      | Some path, In_place -> Out_channel.write_all path ~data:replacement_content
      | _, Stdout -> Format.fprintf ppf "%s" replacement_content
      | Some path, Json_pretty ->
        Format.fprintf ppf "%a" Replacement.pp_json_pretty (Some path, source_content, replacements, replacement_content)
      | Some path, Json_lines -> Format.fprintf ppf "%a" Replacement.pp_json_lines (Some path, source_content, replacements, replacement_content)
      | None, Json_pretty -> Format.fprintf ppf "%a" Replacement.pp_json_pretty (path, source_content, replacements, replacement_content)
      | None, Json_lines -> Format.fprintf ppf "%a" Replacement.pp_json_lines (path, source_content, replacements, replacement_content)
      | Some in_, Diff -> Format.fprintf ppf "%a" Replacement.pp_diff (Some in_, source_content, replacement_content)
      | None, _ -> Format.printf "%s" replacement_content
  end
end

type t =
  { sources : Command_input.t
  ; specifications : Specification.t list
  ; file_extensions : string list option
  ; run_options : run_options
  ; output_printer : Printer.t
  }

let validate_errors { input_options; run_options = _; output_options } =
  let violations =
    [ input_options.stdin && Option.is_some input_options.zip_file
    , "-zip may not be used with stdin."
    ; output_options.in_place && is_some input_options.zip_file
    , "-in-place may not be used with -zip"
    ; output_options.in_place && output_options.stdout
    , "-in-place may not be used with stdout."
    ; output_options.in_place && output_options.diff
    , "-in-place may not be used with -diff"
    ; output_options.in_place && (output_options.json_lines || output_options.json_pretty)
    , "-in-place may not be used with -json-lines or -json-pretty"
    ; input_options.anonymous_arguments = None &&
      (input_options.specification_directories = None
       || input_options.specification_directories = Some []),
      "No templates specified. \
       Either on the command line, or \
       using -templates \
       <directory-containing-templates>"
    ; let result = Rule.create input_options.rule in
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
         ; stdin
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
         ({ in_place
          ; _
          } as output_options)
     } as configuration)
  : t Or_error.t =
  let open Or_error in
  validate_errors configuration >>= fun () ->
  emit_warnings configuration >>= fun () ->
  let specifications =
    match specification_directories, anonymous_arguments with
    | None, Some { match_template; rewrite_template; _ } ->
      if match_only then
        [ Specification.create ~match_template ~match_rule:rule () ]
      else
        [ Specification.create ~match_template ~rewrite_template ~match_rule:rule ~rewrite_rule:rule () ]
    | Some specification_directories, _ ->
      parse_specification_directories match_only specification_directories
    | _ -> assert false
  in
  let file_extensions =
    match anonymous_arguments with
    | None -> file_extensions
    | Some { extensions = None; _ } -> file_extensions
    | Some { extensions = Some file_extensions; _ } -> Some file_extensions
  in
  let input_source =
    match stdin, zip_file with
    | true, _ -> Stdin
    | _, Some _ -> Zip
    | false, None -> Directory
  in
  let sources =
    match input_source with
    | Stdin -> `String (In_channel.input_all In_channel.stdin)
    | Zip -> `Zip (Option.value_exn zip_file)
    | Directory -> `Paths (parse_source_directories ?file_extensions target_directory)
  in
  let in_place = if input_source = Zip || input_source = Stdin then false else in_place in
  let output_options = { output_options with in_place } in
  let output_printer printable =
    let open Printer in
    match printable with
    | Matches { source_path; matches } ->
      Printer.Match.convert output_options
      |> fun match_output ->
      Printer.Match.print match_output source_path matches
    | Replacements { source_path; replacements; result; source_content } ->
      Printer.Rewrite.convert output_options
      |> fun replacement_output ->
      Printer.Rewrite.print replacement_output source_path replacements result source_content
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
