open Core
open Camlzip
open Polymorphic_compare
open Comby_kernel

let debug = Sys.getenv "DEBUG_COMBY" |> Option.is_some

(* skip or continue directory descent *)
type 'a next =
  | Skip of 'a
  | Continue of 'a

let fold_directory ?(sorted = false) root ~init ~f =
  let rec aux acc absolute_path depth =
    if Sys.is_file absolute_path = `Yes then (
      match f acc ~depth ~absolute_path ~is_file:true with
      | Continue acc | Skip acc -> acc)
    else if Sys.is_directory absolute_path = `Yes then (
      match f acc ~depth ~absolute_path ~is_file:false with
      | Skip acc -> acc
      | Continue acc ->
        let dir_contents =
          if Option.is_some (Sys.getenv "COMBY_TEST") || sorted then
            Sys.ls_dir absolute_path |> List.sort ~compare:String.compare |> List.rev
          else
            Sys.ls_dir absolute_path
        in
        List.fold dir_contents ~init:acc ~f:(fun acc subdir ->
          aux acc (Filename.concat absolute_path subdir) (depth + 1)))
    else
      acc
  in
  (* The first valid ls_dir happens at depth 0 *)
  aux init root (-1)

let parse_source_directories
  ?(file_filters = [])
  exclude_directory_prefix
  exclude_file_prefix
  target_directory
  directory_depth
  =
  let max_depth = Option.value directory_depth ~default:Int.max_value in
  let exact_file_paths, file_patterns =
    List.partition_map file_filters ~f:(fun path ->
      let is_exact path =
        (String.contains path '/' && Sys.is_file path = `Yes) || Sys.is_file ("." ^/ path) = `Yes
        (* See if it matches something in the current directory *)
      in
      if is_exact path then Either.First path else Either.Second path)
  in
  let f acc ~depth ~absolute_path ~is_file =
    if depth > max_depth then
      Skip acc
    else if is_file then (
      match file_patterns with
      | [] ->
        let files =
          if
            List.exists exclude_file_prefix ~f:(fun prefix ->
              String.is_prefix (Filename.basename absolute_path) ~prefix)
          then
            acc
          else
            absolute_path :: acc
        in
        Continue files
      | suffixes when List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix absolute_path)
        ->
        let files =
          if
            List.exists exclude_file_prefix ~f:(fun prefix ->
              String.is_prefix (Filename.basename absolute_path) ~prefix)
          then
            acc
          else
            absolute_path :: acc
        in
        Continue files
      | _ -> Continue acc)
    else if
      List.exists exclude_directory_prefix ~f:(fun prefix ->
        String.is_prefix (Filename.basename absolute_path) ~prefix)
    then
      Skip acc
    else
      Continue acc
  in
  let source_paths =
    if (not (List.is_empty file_patterns)) || List.is_empty file_filters then
      fold_directory target_directory ~init:[] ~f
    else
      []
  in
  exact_file_paths @ source_paths

let read filename =
  In_channel.read_all filename
  |> fun template -> String.chop_suffix template ~suffix:"\n" |> Option.value ~default:template

let create_rule ~metasyntax rule =
  match Option.map rule ~f:(Matchers.Rule.create ?metasyntax) with
  | None -> None
  | Some (Ok rule) -> Some rule
  | Some (Error error) ->
    Format.eprintf "Rule parse error: %s@." (Error.to_string_hum error);
    exit 1

let parse_toml ?metasyntax path =
  let open Toml.Types in
  let toml = Toml.Parser.(from_filename path |> unsafe) in
  let toml = Table.remove (Toml.Min.key "flags") toml in
  let to_specification (key : Table.key) (value : Toml.Types.value) acc =
    let name = Table.Key.to_string key in
    if debug then Format.printf "Key name: %s@." name;
    match value with
    | TTable t ->
      let to_string = function
        | None -> None
        | Some (TString s) -> Some s
        | Some v ->
          Format.eprintf "TOML value not a string: %s@." (Toml.Printer.string_of_value v);
          exit 1
      in
      let match_template =
        match Table.find_opt (Toml.Min.key "match") t with
        | Some v -> Option.value_exn (to_string (Some v))
        | None ->
          Format.eprintf "A 'match' key is required for entry %s@." name;
          exit 1
      in
      let rule = Table.find_opt (Toml.Min.key "rule") t |> to_string |> create_rule ~metasyntax in
      let rewrite_template = Table.find_opt (Toml.Min.key "rewrite") t |> to_string in
      if debug then Format.printf "Processed ->%s<-@." match_template;
      (name, Matchers.Specification.create ~match_template ?rule ?rewrite_template ()) :: acc
    | v ->
      Format.eprintf "Unexpected format, could not parse ->%s<-@." (Toml.Printer.string_of_value v);
      exit 1
  in
  Toml.Types.Table.fold to_specification toml []
  |> List.sort ~compare:(fun x y -> String.compare (fst x) (fst y))
  |> List.map ~f:snd

let parse_templates ?metasyntax ?(warn_for_missing_file_in_dir = false) paths =
  let parse_directory path =
    let read_optional filename =
      match read filename with
      | content -> Some content
      | exception _ -> None
    in
    match read_optional (path ^/ "match") with
    | None ->
      if warn_for_missing_file_in_dir then
        Format.eprintf "WARNING: Could not read required match file in %s@." path;
      None
    | Some match_template ->
      let rule = create_rule ~metasyntax @@ read_optional (path ^/ "rule") in
      let rewrite_template = read_optional (path ^/ "rewrite") in
      Matchers.Specification.create ~match_template ?rule ?rewrite_template () |> Option.some
  in
  let f acc ~depth:_ ~absolute_path ~is_file =
    let is_leaf_directory absolute_path =
      (not is_file)
      && Sys.ls_dir absolute_path
         |> List.for_all ~f:(fun path -> Sys.is_directory (absolute_path ^/ path) = `No)
    in
    if is_leaf_directory absolute_path then (
      match parse_directory absolute_path with
      | Some spec -> Continue (spec :: acc)
      | None -> Continue acc)
    else
      Continue acc
  in
  List.concat_map paths ~f:(fun path ->
    if Sys.is_directory path = `Yes then
      fold_directory path ~sorted:true ~init:[] ~f
    else
      parse_toml ?metasyntax path)

type interactive_review =
  { editor : string
  ; default_is_accept : bool
  }

type output_options =
  { color : bool
  ; json_lines : bool
  ; json_only_diff : bool
  ; overwrite_file_in_place : bool
  ; diff : bool
  ; stdout : bool
  ; substitute_in_place : bool
  ; count : bool
  ; interactive_review : interactive_review option
  ; chunk_matches : int option
  }

type anonymous_arguments =
  { match_template : string
  ; rewrite_template : string
  ; file_filters : string list option
  }

type user_input_options =
  { rule : string
  ; stdin : bool
  ; tar : bool
  ; templates : string list option
  ; anonymous_arguments : anonymous_arguments option
  ; file_filters : string list option
  ; zip_file : string option
  ; match_only : bool
  ; target_directory : string
  ; directory_depth : int option
  ; exclude_directory_prefix : string list
  ; exclude_file_prefix : string list
  ; custom_metasyntax : string option
  ; custom_matcher : string option
  ; override_matcher : string option
  ; regex_pattern : bool
  ; ripgrep_args : string option
  ; omega : bool
  }

type compute_mode =
  [ `Sequential
  | `Hack_parallel of int
  | `Parany of int
  ]

type run_options =
  { verbose : bool
  ; match_timeout : int
  ; dump_statistics : bool
  ; disable_substring_matching : bool
  ; fast_offset_conversion : bool
  ; match_newline_toplevel : bool
  ; bound_count : int option
  ; compute_mode : compute_mode
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
  | Tar

module Printer = struct
  type printable_result =
    | Matches of
        { source_path : string option
        ; source_content : string
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
    type match_only_kind =
      | Contents
      | Count
      | Chunk_matches of int option

    type match_output =
      | Json_lines
      | Match_only of match_only_kind

    val convert : output_options -> match_output
    val print : match_output -> string -> string option -> Match.t list -> unit
  end = struct
    type match_only_kind =
      | Contents
      | Count
      | Chunk_matches of int option

    type match_output =
      | Json_lines
      | Match_only of match_only_kind

    let convert output_options =
      match output_options with
      | { chunk_matches = Some _ as n; _ } -> Match_only (Chunk_matches n)
      | { json_lines = true; _ } -> Json_lines
      | { count = true; _ } -> Match_only Count
      | _ -> Match_only Contents

    let print (match_output : match_output) source_content source_path matches =
      if List.length matches = 0 then
        ()
      else (
        let ppf = Format.std_formatter in
        match match_output with
        | Match_only Contents -> Format.fprintf ppf "%a%!" Match.pp (source_path, matches)
        | Match_only Count -> Format.fprintf ppf "%a%!" Match.pp_match_count (source_path, matches)
        | Json_lines -> Format.fprintf ppf "%a%!" Match.pp_json_lines (source_path, matches)
        | Match_only (Chunk_matches threshold) ->
          let chunk_matches = Match.to_chunks ?threshold source_content matches in
          Format.fprintf ppf "%a%!" Match.pp_chunk_matches (source_path, chunk_matches))
  end

  module Rewrite : sig
    type json_kind =
      | Everything
      | Only_diff

    type substitution_kind =
      | Newline_separated
      | In_place

    type diff_kind = Diff_configuration.kind

    type output_format =
      | Interactive_review
      | Overwrite_file
      | Stdout
      | Diff of diff_kind
      | Json_lines of json_kind
      | Match_only

    val convert : output_options -> output_format
    val print : output_format -> string option -> Replacement.t list -> string -> string -> unit
  end = struct
    type diff_kind = Diff_configuration.kind

    type json_kind =
      | Everything
      | Only_diff

    type substitution_kind =
      | Newline_separated
      | In_place

    type output_format =
      | Interactive_review
      | Overwrite_file
      | Stdout
      | Diff of diff_kind
      | Json_lines of json_kind
      | Match_only

    type replacement_output = output_format

    let convert output_options : output_format =
      match output_options with
      | { interactive_review = Some _; _ } -> Interactive_review
      | { overwrite_file_in_place = true; _ } -> Overwrite_file
      | { stdout = true; _ } -> Stdout
      | { json_lines = true; overwrite_file_in_place = false; json_only_diff; _ } ->
        if json_only_diff then
          Json_lines Only_diff
        else
          Json_lines Everything
      | { diff = true; color = false; _ } -> Diff Plain
      | { color = true; _ } | _ -> Diff Colored

    let print output_format path replacements rewritten_source source_content =
      let ppf = Format.std_formatter in
      let print_if_some output =
        Option.value_map output ~default:() ~f:(Format.fprintf ppf "%s@.")
      in
      match output_format with
      | Stdout ->
        if not (String.equal "\n" rewritten_source) then
          (* FIXME: somehow newlines are entering here. *)
          Format.fprintf ppf "%s" rewritten_source
      | Overwrite_file ->
        if replacements <> [] then
          Out_channel.write_all ~data:rewritten_source (Option.value path ~default:"/dev/null")
      | Interactive_review -> () (* Handled after (potentially parallel) processing *)
      | Diff kind ->
        print_if_some @@ Diff_configuration.get_diff kind path source_content rewritten_source
      | Match_only ->
        print_if_some @@ Diff_configuration.get_diff Match_only path rewritten_source source_content
      | Json_lines kind ->
        let open Option in
        let to_json diff =
          let default = Replacement.to_json ?path ~diff in
          match kind with
          | Everything -> default ~replacements ~rewritten_source ()
          | Only_diff -> default ()
        in
        print_if_some
          (Diff_configuration.get_diff Plain path source_content rewritten_source
          >>| fun diff -> Yojson.Safe.to_string @@ to_json diff)
  end
end

type t =
  { sources : Command_input.t
  ; specifications : Matchers.Specification.t list
  ; run_options : run_options
  ; output_printer : Printer.t
  ; interactive_review : interactive_review option
  ; matcher : (module Matchers.Matcher.S)
  ; metasyntax : Matchers.Metasyntax.t option
  ; substitute_in_place : bool
  }

let parse_metasyntax metasyntax_path =
  match metasyntax_path with
  | None -> Matchers.Metasyntax.default_metasyntax
  | Some metasyntax_path ->
    (match Sys.file_exists metasyntax_path with
     | `No | `Unknown ->
       Format.eprintf "Could not open file: %s@." metasyntax_path;
       exit 1
     | `Yes ->
       Yojson.Safe.from_file metasyntax_path
       |> Matchers.Metasyntax.of_yojson
       |> (function
       | Ok c -> c
       | Error error ->
         Format.eprintf "%s@." error;
         exit 1))

let emit_errors { input_options; output_options; _ } =
  let error_on =
    [ ( input_options.stdin && Option.is_some input_options.zip_file
      , "-zip may not be used with -stdin." )
    ; ( output_options.stdout && output_options.diff
      , "-stdout may not be used with -diff. Note: -stdout outputs the changed file contents and \
         -diff outputs a unified diff. Choose one of these." )
    ; ( output_options.overwrite_file_in_place && is_some input_options.zip_file
      , "-in-place may not be used with -zip." )
    ; ( output_options.overwrite_file_in_place && input_options.tar
      , "-in-place may not be used with -tar." )
    ; ( output_options.overwrite_file_in_place && output_options.stdout
      , "-in-place may not be used with -stdout." )
    ; ( output_options.overwrite_file_in_place && output_options.diff
      , "-in-place may not be used with -diff." )
    ; ( Option.is_some output_options.interactive_review
        && (input_options.stdin
           || Option.is_some input_options.zip_file
           || input_options.match_only
           || input_options.tar)
      , "-review cannot be used with one or more of the following input flags: -stdin, -zip, \
         -match-only, -tar." )
    ; ( Option.is_some output_options.interactive_review
        && (output_options.json_lines
           || output_options.json_only_diff
           || output_options.stdout
           || output_options.diff
           || output_options.overwrite_file_in_place
           || output_options.count)
      , "-review cannot be used with one or more of the following output flags: -json-lines, \
         -json-only-diff, -stdout, -in-place, -count" )
    ; ( input_options.anonymous_arguments = None
        && (input_options.templates = None || input_options.templates = Some [])
      , "No templates specified. See -h to specify on the command line, or use -templates \
         <directory-containing-templates>." )
    ; ( Option.is_some input_options.directory_depth
        && Option.value_exn input_options.directory_depth < 0
      , "-depth must be 0 or greater." )
    ; ( Sys.is_directory input_options.target_directory = `No
      , "Directory specified with -d or -directory is not a directory." )
    ; ( output_options.json_only_diff && not output_options.json_lines
      , "-json-only-diff can only be supplied with -json-lines." )
    ; ( Option.is_some output_options.chunk_matches && Option.is_some input_options.zip_file
      , "chunk-matches output format is not supported for zip files." )
    ; ( Option.is_some output_options.interactive_review
        && not (String.equal input_options.target_directory (Sys.getcwd ()))
      , "Please remove the -d option and `cd` to the directory where you want to review from. The \
         -review, -editor, or -default-no options should only be run at the root directory of the \
         project files to patch." )
    ; (let message =
         match input_options.templates with
         | Some inputs ->
           List.find_map inputs ~f:(fun input ->
             if Sys.is_file input = `Yes then (
               match Toml.Parser.from_filename input with
               | `Error (s, _) -> Some s
               | _ -> None)
             else if not (Sys.is_directory input = `Yes) then
               Some
                 (Format.sprintf "Directory %S specified with -templates is not a directory." input)
             else
               None)
         | _ -> None
       in
       ( Option.is_some message
       , if Option.is_some message then
           Option.value_exn message
         else
           "UNREACHABLE" ))
    ; (let result =
         Matchers.Rule.create
           ~metasyntax:(parse_metasyntax input_options.custom_metasyntax)
           input_options.rule
       in
       ( Or_error.is_error result
       , if Or_error.is_error result then
           Format.sprintf "Match rule parse error: %s@."
           @@ Error.to_string_hum (Option.value_exn (Result.error result))
         else
           "UNREACHABLE" ))
    ]
  in
  List.filter_map error_on ~f:(function
    | true, message -> Some (Or_error.error_string message)
    | _ -> None)
  |> Or_error.combine_errors_unit
  |> Result.map_error ~f:(fun error ->
       let message =
         let rec to_string acc = function
           | Sexp.Atom s -> s
           | List [] -> ""
           | List (x :: []) -> to_string acc x
           | List (x :: xs) ->
             List.fold xs ~init:acc ~f:to_string ^ "\nNext error: " ^ to_string acc x
         in
         Error.to_string_hum error |> Sexp.of_string |> to_string ""
       in
       Error.of_string message)

let emit_warnings { input_options; output_options; _ } =
  let warn_on =
    [ ( (let match_templates =
           match input_options.templates, input_options.anonymous_arguments with
           | None, Some ({ match_template; _ } : anonymous_arguments) -> [ match_template ]
           | Some templates, _ ->
             List.map
               (parse_templates
                  ~metasyntax:(parse_metasyntax input_options.custom_metasyntax)
                  templates)
               ~f:(fun { match_template; _ } -> match_template)
           | _ -> assert false
         in
         List.exists match_templates ~f:(fun match_template ->
           Pcre.(pmatch ~rex:(regexp "^:\\[[[:alnum:]]+\\]") match_template)))
      , "The match template starts with a :[hole]. You almost never want to start a template with \
         :[hole], since it matches everything including newlines up to the part that comes after \
         it. This can make things slow. :[[hole]] might be what you're looking for instead, like \
         when you want to match an assignment foo = bar(args) on a line, use :[[var]] = bar(args). \
         :[hole] is typically useful inside balanced delimiters." )
    ; ( is_some input_options.templates && is_some input_options.anonymous_arguments
      , "Templates specified on the command line AND using -templates. Ignoring match\n\
        \      and rewrite templates on the command line and only using those in directories." )
    ; ( output_options.color
        && (output_options.stdout
           || output_options.json_lines
           || output_options.overwrite_file_in_place)
      , "-color only works with -diff." )
    ; ( output_options.count && not input_options.match_only
      , "-count only works with -match-only. Performing -match-only -count." )
    ; ( input_options.stdin && output_options.overwrite_file_in_place
      , "-in-place has no effect when -stdin is used. Ignoring -in-place." )
    ; ( output_options.count && output_options.json_lines
      , "-count and -json-lines is specified. Ignoring -count." )
    ; input_options.stdin && input_options.tar, "-tar implies -stdin. Ignoring -stdin."
    ; ( Option.is_some output_options.chunk_matches && not (input_options.stdin || input_options.tar)
      , "printing chunk match format for output option that is NOT -stdin nor -tar. This is very \
         inefficient!" )
    ]
  in
  List.iter warn_on ~f:(function
    | true, message -> Format.eprintf "WARNING: %s@." message
    | _ -> ());
  Ok ()

let with_zip zip_file ~f =
  let zip_in = Zip.open_in zip_file in
  let result = f zip_in in
  Zip.close_in zip_in;
  result

let filter_zip_entries file_filters exclude_directory_prefix exclude_file_prefix zip =
  let exclude_the_directory prefixes filename =
    List.exists prefixes ~f:(fun prefix -> String.is_prefix ~prefix filename)
  in
  let exclude_the_file prefixes filename =
    List.exists prefixes ~f:(fun prefix -> String.is_prefix ~prefix filename)
  in
  match file_filters with
  | Some [] | None ->
    List.filter (Zip.entries zip) ~f:(fun { is_directory; filename; _ } ->
      (not is_directory)
      && (not (exclude_the_directory exclude_directory_prefix filename))
      && not (exclude_the_file exclude_file_prefix (Filename.basename filename)))
  | Some suffixes ->
    let has_acceptable_suffix filename =
      List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename)
    in
    List.filter (Zip.entries zip) ~f:(fun { is_directory; filename; _ } ->
      (not is_directory)
      && (not (exclude_the_directory exclude_directory_prefix filename))
      && (not (exclude_the_file exclude_file_prefix (Filename.basename filename)))
      && has_acceptable_suffix filename)

let syntax custom_matcher_path =
  match Sys.file_exists custom_matcher_path with
  | `No | `Unknown ->
    Format.eprintf "Could not open file: %s@." custom_matcher_path;
    exit 1
  | `Yes ->
    Yojson.Safe.from_file custom_matcher_path
    |> Matchers.Language.Syntax.of_yojson
    |> (function
    | Ok c -> c
    | Error error ->
      Format.eprintf "%s@." error;
      exit 1)

let force_language language =
  match Matchers.Languages.select_with_extension language with
  | Some matcher -> matcher
  | None when language <> ".generic" ->
    Format.eprintf "The matcher %S is not supported. See -list for supported matchers@." language;
    exit 1
  | None -> (module Matchers.Languages.Generic)

let extension file_filters =
  match file_filters with
  | None | Some [] -> ".generic"
  | Some (filter :: _) ->
    (match Filename.split_extension filter with
     | _, Some extension -> "." ^ extension
     | extension, None -> "." ^ extension)

let of_extension
  (module Engine : Matchers.Engine.S)
  (module External : Matchers.External.S)
  file_filters
  =
  let external_handler = External.handler in
  let extension = extension file_filters in
  match Engine.select_with_extension extension ~external_handler with
  | Some matcher -> matcher, Some extension, None
  | None -> (module Engine.Generic), Some extension, None

let select_matcher custom_metasyntax custom_matcher override_matcher file_filters omega =
  let (module Engine : Matchers.Engine.S) =
    if omega then
      (module Matchers.Omega)
    else
      (module Matchers.Alpha)
  in
  let module External = struct
    let handler = External_semantic.lsif_hover
  end
  in
  if debug then Format.printf "Set custom external@.";
  match custom_matcher, override_matcher, custom_metasyntax with
  | Some custom_matcher, _, custom_metasyntax ->
    (* custom matcher, optional custom metasyntax *)
    let metasyntax = parse_metasyntax custom_metasyntax in
    let syntax = syntax custom_matcher in
    if debug then Format.printf "Engine.create@.";
    Engine.create ~metasyntax syntax, None, Some metasyntax
  | _, Some language, custom_metasyntax ->
    (* forced language, optional custom metasyntax *)
    let metasyntax = parse_metasyntax custom_metasyntax in
    let (module Metasyntax) = Matchers.Metasyntax.create metasyntax in
    let (module Language) = force_language language in
    if debug then Format.printf "Engine.Make@.";
    ( (module Engine.Make (Language) (Metasyntax) (External) : Matchers.Matcher.S)
    , None
    , Some metasyntax )
  | _, _, Some custom_metasyntax ->
    (* infer language from file filters, definite custom metasyntax *)
    let metasyntax = parse_metasyntax (Some custom_metasyntax) in
    let (module Metasyntax) = Matchers.Metasyntax.create metasyntax in
    let (module Language) = force_language (extension file_filters) in
    if debug then Format.printf "Engine.Make2@.";
    ( (module Engine.Make (Language) (Metasyntax) (External) : Matchers.Matcher.S)
    , None
    , Some metasyntax )
  | _, _, None ->
    (* infer language from file filters, use default metasyntax *)
    if debug then Format.printf "Engine.Infer@.";
    of_extension (module Engine) (module External) file_filters

let regex_of_specifications specifications =
  Format.sprintf "(%s)"
  @@ String.concat ~sep:")|("
  @@ List.map specifications ~f:Matchers.Specification.to_regex

let ripgrep_file_filters specifications args : string list =
  let regex = regex_of_specifications specifications in
  let args =
    String.split_on_chars args ~on:[ ' '; '\t'; '\r'; '\n' ] |> List.filter ~f:(String.( <> ) "")
  in
  let result = Ripgrep.run ~pattern:regex ~args in
  match result with
  | Ok (Some result) ->
    if debug then Format.printf "Ripgrep result: %s@." @@ String.concat ~sep:"\n" result;
    result
  | Ok None ->
    if debug then Format.printf "No ripgrep results, no files to process. Exiting 0.";
    exit 0
  | Error e ->
    if debug then Format.eprintf "%s@." (Error.to_string_hum e);
    exit 1

let create
  ({ input_options =
       { rule
       ; templates
       ; anonymous_arguments
       ; file_filters
       ; zip_file
       ; match_only
       ; stdin
       ; tar
       ; target_directory
       ; directory_depth
       ; exclude_directory_prefix
       ; exclude_file_prefix
       ; custom_metasyntax
       ; custom_matcher
       ; override_matcher
       ; regex_pattern
       ; ripgrep_args
       ; omega
       }
   ; run_options
   ; output_options =
       { overwrite_file_in_place; color; count; interactive_review; substitute_in_place; _ } as
       output_options
   } as configuration)
  : t Or_error.t
  =
  let open Or_error in
  emit_errors configuration
  >>= fun () ->
  emit_warnings configuration
  >>= fun () ->
  let rule =
    Matchers.Rule.create ~metasyntax:(parse_metasyntax custom_metasyntax) rule |> Or_error.ok_exn
  in
  let specifications =
    match templates, anonymous_arguments with
    | None, Some { match_template; rewrite_template; _ } ->
      if match_only || count then
        [ Matchers.Specification.create ~match_template ~rule () ]
      else
        [ Matchers.Specification.create ~match_template ~rewrite_template ~rule () ]
    | Some templates, _ -> parse_templates ~warn_for_missing_file_in_dir:true templates
    | _ -> assert false
  in
  let specifications =
    if match_only then
      List.map specifications ~f:(fun { match_template; rule; _ } ->
        Matchers.Specification.create ~match_template ?rule ())
    else
      specifications
  in
  if regex_pattern then (
    Format.printf "%s@." (regex_of_specifications specifications);
    exit 0);
  let file_filters_from_anonymous_args =
    match anonymous_arguments with
    | None -> file_filters
    | Some { file_filters = None; _ } -> file_filters
    | Some { file_filters = Some anonymous_file_filters; _ } ->
      (match file_filters with
       | Some additional_file_filters -> Some (additional_file_filters @ anonymous_file_filters)
       | None -> Some anonymous_file_filters)
  in
  let file_filters =
    match ripgrep_args with
    | Some args -> Some (ripgrep_file_filters specifications args)
    | None -> file_filters_from_anonymous_args
  in
  let input_source =
    match stdin, zip_file, tar with
    | _, _, true -> Tar
    | true, _, _ -> Stdin
    | _, Some _, _ -> Zip
    | false, None, _ -> Directory
  in
  let sources =
    match input_source with
    | Stdin -> `String (In_channel.input_all In_channel.stdin)
    | Tar -> `Tar
    | Zip ->
      let zip_file = Option.value_exn zip_file in
      let paths : Zip.entry list =
        with_zip
          zip_file
          ~f:(filter_zip_entries file_filters exclude_directory_prefix exclude_file_prefix)
      in
      `Zip (zip_file, paths)
    | Directory ->
      let target_directory =
        if target_directory = "." then
          Filename.realpath target_directory
        else
          target_directory
      in
      let paths =
        parse_source_directories
          ?file_filters
          exclude_directory_prefix
          exclude_file_prefix
          target_directory
          directory_depth
      in
      `Paths paths
  in
  let overwrite_file_in_place =
    if input_source = Zip || input_source = Stdin || input_source = Tar then
      false
    else
      overwrite_file_in_place
  in
  let output_options = { output_options with overwrite_file_in_place } in
  let output_printer printable =
    let open Printer in
    match printable with
    | Matches { source_path; matches; source_content } ->
      Printer.Match.convert output_options
      |> fun match_output -> Printer.Match.print match_output source_content source_path matches
    | Replacements { source_path; replacements; result; source_content } ->
      Printer.Rewrite.convert output_options
      |> fun replacement_output ->
      if match_only && color then
        Printer.Rewrite.print Match_only source_path replacements result source_content
      else
        Printer.Rewrite.print replacement_output source_path replacements result source_content
  in
  let ((module M) as matcher), _, metasyntax =
    select_matcher custom_metasyntax custom_matcher override_matcher file_filters omega
  in
  return
    { matcher
    ; sources
    ; specifications
    ; run_options
    ; output_printer
    ; interactive_review
    ; metasyntax
    ; substitute_in_place
    }
