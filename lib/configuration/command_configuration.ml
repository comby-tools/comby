open Core
open Polymorphic_compare

open Language

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

(* skip or continue directory descent *)
type 'a next =
  | Skip of 'a
  | Continue of 'a

let fold_directory ?(sorted=false) root ~init ~f =
  let rec aux acc absolute_path depth =
    if Sys.is_file absolute_path = `Yes then
      match f acc ~depth ~absolute_path ~is_file:true with
      | Continue acc
      | Skip acc -> acc
    else if Sys.is_directory absolute_path = `Yes then
      match f acc ~depth ~absolute_path ~is_file:false with
      | Skip acc -> acc
      | Continue acc ->
        let dir_contents =
          if Option.is_some (Sys.getenv "COMBY_TEST") || sorted then
            Sys.ls_dir absolute_path
            |> List.sort ~compare:String.compare
            |> List.rev
          else
            Sys.ls_dir absolute_path
        in
        List.fold dir_contents ~init:acc ~f:(fun acc subdir ->
            aux acc (Filename.concat absolute_path subdir) (depth + 1))
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
    directory_depth =
  let max_depth = Option.value directory_depth ~default:Int.max_value in
  let f acc ~depth ~absolute_path ~is_file =
    if depth > max_depth then
      Skip acc
    else
      begin
        if is_file then
          match file_filters with
          | [] ->
            let files =
              if List.exists exclude_file_prefix ~f:(fun prefix -> String.is_prefix (Filename.basename absolute_path) ~prefix) then
                acc
              else
                absolute_path::acc
            in
            Continue files
          | suffixes when List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix absolute_path) ->
            let files =
              if List.exists exclude_file_prefix  ~f:(fun prefix -> String.is_prefix (Filename.basename absolute_path) ~prefix) then
                acc
              else
                absolute_path::acc
            in
            Continue files
          | _ ->
            Continue acc
        else
          begin
            if List.exists exclude_directory_prefix
                ~f:(fun prefix -> String.is_prefix (Filename.basename absolute_path) ~prefix) then
              Skip acc
            else
              Continue acc
          end
      end
  in
  fold_directory target_directory ~init:[] ~f

let read filename =
  In_channel.read_all filename
  |> fun template ->
  String.chop_suffix template ~suffix:"\n"
  |> Option.value ~default:template

let parse_template_directories ?(warn = false) paths =
  let parse_directory path =
    let read_optional filename =
      match read filename with
      | content -> Some content
      | exception _ -> None
    in
    match read_optional (path ^/ "match") with
    | None ->
      if warn then Format.eprintf "WARNING: Could not read required match file in %s@." path;
      None
    | Some match_template ->
      let rule = read_optional (path ^/ "rule") in
      let rule =
        match Option.map rule ~f:Rule.create with
        | None -> None
        | Some Ok rule -> Some rule
        | Some Error error ->
          Format.eprintf "Rule parse error: %s@." (Error.to_string_hum error);
          exit 1
      in
      let rewrite_template = read_optional (path ^/ "rewrite") in
      Specification.create ~match_template ?rule ?rewrite_template ()
      |> Option.some
  in
  let f acc ~depth:_ ~absolute_path ~is_file =
    let is_leaf_directory absolute_path =
      not is_file &&
      Sys.ls_dir absolute_path
      |> List.for_all ~f:(fun path -> Sys.is_directory (absolute_path ^/ path) = `No)
    in
    if is_leaf_directory absolute_path then
      match parse_directory absolute_path with
      | Some spec -> Continue (spec::acc)
      | None -> Continue acc
    else
      Continue acc
  in
  List.concat_map paths ~f:(fun path -> fold_directory path ~sorted:true ~init:[] ~f)

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
  }

type anonymous_arguments =
  { match_template : string
  ; rewrite_template : string
  ; file_filters : string list option
  }

type user_input_options =
  { rule : string
  ; stdin : bool
  ; specification_directories : string list option
  ; anonymous_arguments : anonymous_arguments option
  ; file_filters : string list option
  ; zip_file : string option
  ; match_only : bool
  ; target_directory : string
  ; directory_depth : int option
  ; exclude_directory_prefix : string list
  ; exclude_file_prefix : string list
  ; custom_matcher : string option
  ; override_matcher : string option
  }

type run_options =
  { sequential : bool
  ; verbose : bool
  ; match_timeout : int
  ; number_of_workers : int
  ; dump_statistics : bool
  ; substitute_in_place : bool
  ; disable_substring_matching : bool
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

    type match_only_kind =
      | Contents
      | Count

    type match_output =
      | Json_lines
      | Match_only of match_only_kind

    val convert : output_options -> match_output

    val print : match_output -> string option -> Match.t list -> unit

  end = struct

    type match_only_kind =
      | Contents
      | Count

    type match_output =
      | Json_lines
      | Match_only of match_only_kind

    let convert output_options =
      match output_options with
      | { json_lines = true; _ } -> Json_lines
      | { count = true; _ } -> Match_only Count
      | _ -> Match_only Contents

    let print (match_output : match_output) source_path matches =
      let ppf = Format.std_formatter in
      let pp =
        match match_output with
        | Match_only Contents -> Match.pp
        | Match_only Count -> Match.pp_match_count
        | Json_lines -> Match.pp_json_lines
      in
      if List.length matches > 0 then
        Format.fprintf ppf "%a@." pp (source_path, matches)

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

    type replacement_output =
      { output_format : output_format
      ; substitution_kind : substitution_kind
      }

    val convert : output_options -> replacement_output

    val print : replacement_output -> string option -> Replacement.t list -> string -> string -> unit

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

    type replacement_output =
      { output_format : output_format
      ; substitution_kind : substitution_kind
      }

    let convert output_options : replacement_output =
      let output_format =
        match output_options with
        | { interactive_review = Some _; _ } -> Interactive_review
        | { overwrite_file_in_place = true; _ } -> Overwrite_file
        | { stdout = true; _ } -> Stdout
        | { json_lines = true; overwrite_file_in_place = false; json_only_diff; _ } ->
          if json_only_diff then
            Json_lines Only_diff
          else
            Json_lines Everything
        | { diff = true; color = false; _ } ->
          Diff Plain
        | { color = true; _ }
        | _ ->
          Diff Colored
      in
      if output_options.substitute_in_place then
        { output_format; substitution_kind = In_place }
      else
        { output_format; substitution_kind = Newline_separated }

    let print { output_format; substitution_kind } path replacements rewritten_source source_content =
      let open Replacement in
      let ppf = Format.std_formatter in
      let rewritten_source =
        match substitution_kind with
        | In_place -> rewritten_source
        | Newline_separated ->
          List.rev_map replacements ~f:(fun { replacement_content; _ } -> replacement_content)
          |> String.concat ~sep:"\n"
          |> Format.sprintf "%s\n"
      in
      let print_if_some output = Option.value_map output ~default:() ~f:(Format.fprintf ppf "%s@.") in
      match output_format with
      | Stdout -> Format.fprintf ppf "%s" rewritten_source
      | Overwrite_file -> Out_channel.write_all ~data:rewritten_source (Option.value path ~default:"/dev/null")
      | Interactive_review -> () (* Handled after (potentially parallel) processing *)
      | Diff kind -> print_if_some @@ Diff_configuration.get_diff kind path source_content rewritten_source
      | Match_only -> print_if_some @@ Diff_configuration.get_diff Match_only path rewritten_source source_content
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
  ; specifications : Specification.t list
  ; run_options : run_options
  ; output_printer : Printer.t
  ; interactive_review : interactive_review option
  ; matcher : (module Matchers.Matcher)
  ; extension : string option
  }

let emit_errors { input_options; run_options = _; output_options } =
  let error_on =
    [ input_options.stdin && Option.is_some input_options.zip_file
    , "-zip may not be used with -stdin."
    ; output_options.stdout && output_options.diff
    , "-stdout may not be used with -diff. Note: -stdout outputs the changed \
       file contents and -diff outputs a unified diff. Choose one of these."
    ; output_options.overwrite_file_in_place && is_some input_options.zip_file
    , "-in-place may not be used with -zip."
    ; output_options.overwrite_file_in_place && output_options.stdout
    , "-in-place may not be used with -stdout."
    ; output_options.overwrite_file_in_place && output_options.diff
    , "-in-place may not be used with -diff."
    ; Option.is_some output_options.interactive_review
      && (input_options.stdin || Option.is_some input_options.zip_file || input_options.match_only)
    , "-review cannot be used with one or more of the following input flags: -stdin, -zip, -match-only."
    ; Option.is_some output_options.interactive_review
      && (output_options.json_lines
          || output_options.json_only_diff
          || output_options.stdout
          || output_options.diff
          || output_options.overwrite_file_in_place
          || output_options.count)
    , "-review cannot be used with one or more of the following output flags: -json-lines, -json-only-diff, -stdout, -in-place, -count"
    ; input_options.anonymous_arguments = None &&
      (input_options.specification_directories = None
       || input_options.specification_directories = Some [])
    , "No templates specified. \
       See -h to specify on the command line, or \
       use -templates \
       <directory-containing-templates>."
    ; Option.is_some input_options.directory_depth
      && Option.value_exn (input_options.directory_depth) < 0
    , "-depth must be 0 or greater."
    ; Sys.is_directory input_options.target_directory = `No
    , "Directory specified with -d or -directory is not a directory."
    ; Option.is_some input_options.specification_directories
      && List.exists
        (Option.value_exn input_options.specification_directories)
        ~f:(fun dir -> not (Sys.is_directory dir = `Yes))
    , "One or more directories specified with -templates is not a directory."
    ; output_options.json_only_diff && not output_options.json_lines
    , "-json-only-diff can only be supplied with -json-lines."
    ; Option.is_some output_options.interactive_review &&
      (not (String.equal input_options.target_directory (Sys.getcwd ())))
    , "Please remove the -d option and `cd` to the directory where you want to \
       review from. The -review, -editor, or -default-no options should only be run \
       at the root directory of the project files to patch."
    ; let result = Rule.create input_options.rule in
      Or_error.is_error result
    , if Or_error.is_error result then
        Format.sprintf "Match rule parse error: %s@." @@
        Error.to_string_hum (Option.value_exn (Result.error result))
      else
        "UNREACHABLE"
    ]
  in
  List.filter_map error_on ~f:(function
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
    [ (let match_templates =
         match input_options.specification_directories, input_options.anonymous_arguments with
         | None, Some { match_template; _ } ->
           [ match_template ]
         | Some specification_directories, _ ->
           List.map (parse_template_directories specification_directories) ~f:(fun { match_template; _ } -> match_template)
         | _ -> assert false
       in
       List.exists match_templates ~f:(fun match_template ->
           Pcre.(pmatch ~rex:(regexp "^:\\[[[:alnum:]]+\\]") match_template))),
      "The match template starts with a :[hole]. You almost never want to start \
       a template with :[hole], since it matches everything including newlines \
       up to the part that comes after it. This can make things slow. :[[hole]] \
       might be what you're looking for instead, like when you want to match an \
       assignment foo = bar(args) on a line, use :[[var]] = bar(args). :[hole] is \
       typically useful inside balanced delimiters."
    ; is_some input_options.specification_directories
      && is_some input_options.anonymous_arguments,
      "Templates specified on the command line AND using -templates. Ignoring match
      and rewrite templates on the command line and only using those in directories."
    ; output_options.color
      && (output_options.stdout
          || output_options.json_lines
          || output_options.overwrite_file_in_place)
    , "-color only works with -diff."
    ; output_options.count && not input_options.match_only
    , "-count only works with -match-only. Performing -match-only -count."
    ; input_options.stdin && output_options.overwrite_file_in_place
    , "-in-place has no effect when -stdin is used. Ignoring -in-place."
    ; output_options.count && output_options.json_lines
    , "-count and -json-lines is specified. Ignoring -count."
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
  | Some [] | None -> List.filter (Zip.entries zip) ~f:(fun { is_directory; filename; _ } ->
      not is_directory
      && not (exclude_the_directory exclude_directory_prefix filename)
      && not (exclude_the_file exclude_file_prefix (Filename.basename filename)))
  | Some suffixes ->
    let has_acceptable_suffix filename =
      List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename)
    in
    List.filter (Zip.entries zip) ~f:(fun { is_directory; filename; _ } ->
        not is_directory
        && not (exclude_the_directory exclude_directory_prefix filename)
        && not (exclude_the_file exclude_file_prefix (Filename.basename filename))
        && has_acceptable_suffix filename)

let select_matcher custom_matcher override_matcher file_filters =
  if Option.is_some custom_matcher then
    let matcher_path = Option.value_exn custom_matcher in
    match Sys.file_exists matcher_path with
    | `No | `Unknown ->
      Format.eprintf "Could not open file: %s@." matcher_path;
      exit 1
    | `Yes ->
      Yojson.Safe.from_file matcher_path
      |> Matchers.Syntax.of_yojson
      |> function
      | Ok c -> Matchers.create c, None
      | Error error ->
        Format.eprintf "%s@." error;
        exit 1
  else if Option.is_some override_matcher then
    let matcher_override = Option.value_exn override_matcher in
    let matcher =
      match Matchers.select_with_extension matcher_override with
      | Some matcher -> matcher
      | None when matcher_override <> ".generic" ->
        Format.eprintf "The matcher %S is not supported. See -list for supported matchers@." matcher_override;
        exit 1
      | None -> (module Matchers.Generic)
    in
    matcher, None
  else
    let extension =
      match file_filters with
      | None | Some [] -> ".generic"
      | Some (filter::_) ->
        match Filename.split_extension filter with
        | _, Some extension -> "." ^ extension
        | extension, None -> "." ^ extension
    in
    match Matchers.select_with_extension extension with
    | Some matcher -> matcher, Some extension
    | None -> (module Matchers.Generic), Some extension

let create
    ({ input_options =
         { rule
         ; specification_directories
         ; anonymous_arguments
         ; file_filters
         ; zip_file
         ; match_only
         ; stdin
         ; target_directory
         ; directory_depth
         ; exclude_directory_prefix
         ; exclude_file_prefix
         ; custom_matcher
         ; override_matcher
         }
     ; run_options =
         { sequential
         ; verbose
         ; match_timeout
         ; number_of_workers
         ; dump_statistics
         ; substitute_in_place
         ; disable_substring_matching
         }
     ; output_options =
         ({ overwrite_file_in_place
          ; color
          ; count
          ; interactive_review
          ; _
          } as output_options)
     } as configuration)
  : t Or_error.t =
  let open Or_error in
  emit_errors configuration >>= fun () ->
  emit_warnings configuration >>= fun () ->
  let rule = Rule.create rule |> Or_error.ok_exn in
  let specifications =
    match specification_directories, anonymous_arguments with
    | None, Some { match_template; rewrite_template; _ } ->
      if match_only || count then
        [ Specification.create ~match_template ~rule () ]
      else
        [ Specification.create ~match_template ~rewrite_template ~rule () ]
    | Some specification_directories, _ ->
      parse_template_directories ~warn:true specification_directories
    | _ -> assert false
  in
  let file_filters =
    match anonymous_arguments with
    | None -> file_filters
    | Some { file_filters = None; _ } -> file_filters
    | Some { file_filters = Some anonymous_file_filters; _ } ->
      match file_filters with
      | Some additional_file_filters ->
        Some (additional_file_filters @ anonymous_file_filters)
      | None ->
        Some anonymous_file_filters
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
    | Zip ->
      let zip_file = Option.value_exn zip_file in
      let paths : Zip.entry list = with_zip zip_file ~f:(filter_zip_entries file_filters exclude_directory_prefix exclude_file_prefix) in
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
    if input_source = Zip || input_source = Stdin then
      false
    else
      overwrite_file_in_place
  in
  let output_options = { output_options with overwrite_file_in_place } in
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
      if match_only && color then
        Printer.Rewrite.print { replacement_output with output_format = Match_only } source_path replacements result source_content
      else
        Printer.Rewrite.print replacement_output source_path replacements result source_content
  in
  let (module M) as matcher, extension = select_matcher custom_matcher override_matcher file_filters in
  return
    { matcher
    ; extension
    ; sources
    ; specifications
    ; run_options =
        { sequential
        ; verbose
        ; match_timeout
        ; number_of_workers
        ; dump_statistics
        ; substitute_in_place
        ; disable_substring_matching
        }
    ; output_printer
    ; interactive_review
    }
