open Core
open Command.Let_syntax

open Hack_parallel

open Command_configuration
open Command_input
open Matchers
open Match
open Language
open Rewriter
open Statistics

type json_result =
  { matches : Match.t list
  ; source : string
  }
[@@deriving yojson]

type processed_source_result =
  | Matches of (Match.t list * int)
  | Replacement of (Replacement.t list * string * int)
  | Nothing

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let verbose_out_file = "/tmp/comby.out"

let get_matches (module Matcher : Matchers.Matcher) configuration match_template rule source =
  let rule = Rule.create rule |> Or_error.ok_exn in
  Matcher.all ~configuration ~template:match_template ~source
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule ~matcher:(module Matcher) environment))

let apply_rewrite_rule newline_separated matcher rewrite_rule matches =
  let open Option in
  match rewrite_rule with
  | "" -> matches
  | rewrite_rule ->
    match Rule.create rewrite_rule with
    | Ok rule ->
      List.filter_map matches ~f:(fun ({ environment; _ } as match_) ->
          let inferred_equality_constraints =
            let vars = Environment.vars environment in
            List.fold vars ~init:[] ~f:(fun acc var ->
                if String.is_prefix var ~prefix:"equal_" then
                  match String.split var ~on:'_' with
                  | _equal :: target :: _uuid ->
                    let expression = Language.Ast.Equal (Variable var, Variable target) in
                    expression::acc
                  | _ -> assert false
                else
                  acc)
          in
          let sat, env = Rule.apply (rule @ inferred_equality_constraints) ~newline_separated ~matcher environment in
          (if sat then env else None)
          >>| fun environment -> { match_ with environment })
    | Error _ -> []

let process_single_source
    ((module Matcher : Matchers.Matcher) as matcher)
    newline_separate_rule_rewrites
    configuration
    source
    specification
    verbose
    match_timeout =
  let open Specification in
  try
    let input_text =
      match source with
      | `String input_text -> input_text
      | `Path path ->
        if verbose then
          Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
              Out_channel.output_lines out_channel [Format.sprintf "Processing %s%!" path]);
        In_channel.read_all path
    in
    match specification with
    | { match_specification = { match_template; rule }
      ; rewrite_specification = None
      } ->
      let matches =
        try
          let f () =
            let get_matches (module Matcher : Matchers.Matcher) configuration match_template rule source =
              let rule = Rule.create rule |> Or_error.ok_exn in
              Matcher.all ~configuration ~template:match_template ~source
              |> List.filter ~f:(fun { environment; _ } ->
                  let inferred_equality_constraints =
                    let vars = Environment.vars environment in
                    List.fold vars ~init:[] ~f:(fun acc var ->
                        if String.is_prefix var ~prefix:"equal_" then
                          match String.split var ~on:'_' with
                          | _equal :: target :: _uuid ->
                            let expression = Language.Ast.Equal (Variable var, Variable target) in
                            expression::acc
                          | _ -> assert false
                        else
                          acc)
                  in
                  Rule.(sat @@ apply (rule @ inferred_equality_constraints) ~matcher:(module Matcher) environment))
            in
            get_matches matcher configuration match_template rule input_text
          in
          Statistics.Time.time_out ~after:match_timeout f ();
        with Statistics.Time.Time_out ->
          Format.eprintf "Timeout for input: %s!@." (show_input_kind source);
          Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
              Out_channel.output_lines out_channel [Format.sprintf "TIMEOUT: %s@." (show_input_kind source) ]);
          []
      in
      Matches (matches, List.length matches)
    | { match_specification = { match_template; _ }
      ; rewrite_specification = Some { rewrite_template; rule }
      } ->
      let result =
        try
          let f () =
            Matcher.all ~configuration ~template:match_template ~source:input_text
            |> fun matches ->
            (* TODO(RVT): merge match and rewrite rule application. *)
            apply_rewrite_rule newline_separate_rule_rewrites matcher rule matches
            |> fun matches ->
            if matches = [] then
              (* If there are no matches, return the original source (for editor support). *)
              Some (Some (Replacement.{ rewritten_source = input_text; in_place_substitutions = [] }), [])
            else
              Some (Rewrite.all ~source:input_text ~rewrite_template matches, matches)
          in
          Statistics.Time.time_out ~after:match_timeout f ();
        with Statistics__Time.Time_out ->
          Format.eprintf "Timeout for input: %s!@." (show_input_kind source);
          Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
              Out_channel.output_lines out_channel [Format.sprintf "TIMEOUT: FOR %s@." (show_input_kind source) ]);
          None
      in
      result
      |> function
      | Some (Some { rewritten_source; in_place_substitutions }, matches) ->
        Replacement (in_place_substitutions, rewritten_source, List.length matches)
      | Some (None, _)
      | None -> Nothing
  with
  | _ ->
    Nothing

let output_result output_printer source_path source_content result =
  match result with
  | Nothing -> ()
  | Matches (matches, _) ->
    output_printer (Printer.Matches { source_path; matches })
  | Replacement (replacements, result, _) ->
    let source_content =
      match source_content with
      | `String content -> content
      | `Path path -> In_channel.read_all path
    in
    output_printer (Printer.Replacements { source_path; replacements; result; source_content })

let select_matcher custom_matcher override_matcher configuration =
  if Option.is_some custom_matcher then
    let matcher_path = Option.value_exn custom_matcher in
    match Sys.file_exists matcher_path with
    | `No | `Unknown ->
      Format.eprintf "Could not open file: %s@." matcher_path;
      exit 1
    | `Yes ->
      Yojson.Safe.from_file matcher_path
      |> Matchers.Syntax_config.of_yojson
      |> function
      | Ok c -> Matchers.create c, None
      | Error error ->
        Format.eprintf "%s@." error;
        exit 1
  else if Option.is_some override_matcher then
    Matchers.select_with_extension (Option.value_exn override_matcher), None
  else
    let extension =
      match configuration.file_filters with
      | None | Some [] -> ".generic"
      | Some (filter::_) ->
        match Filename.split_extension filter with
        | _, Some extension -> "." ^ extension
        | extension, None -> "." ^ extension
    in
    Matchers.select_with_extension extension, Some extension

let write_statistics number_of_matches paths total_time dump_statistics =
  if dump_statistics then
    let total_time = Statistics.Time.stop total_time in
    let lines_of_code =
      List.fold paths ~init:0 ~f:(fun acc paths ->
          In_channel.read_lines paths
          |> List.length
          |> (+) acc)
    in
    let statistics =
      { number_of_files = List.length paths
      ; lines_of_code
      ; number_of_matches
      ; total_time = total_time
      }
    in
    Format.eprintf "%s@."
    @@ Yojson.Safe.pretty_to_string
    @@ Statistics.to_yojson statistics
  else
    ()

let paths_with_file_size paths =
  List.map paths ~f:(fun path ->
      let length =
        In_channel.create path
        |> fun channel ->
        In_channel.length channel
        |> Int64.to_int
        |> (fun value -> Option.value_exn value)
        |> (fun value -> In_channel.close channel; value)
      in
      (path, length))

let run
    matcher
    { sources
    ; specifications
    ; file_filters
    ; exclude_directory_prefix
    ; run_options =
        { sequential
        ; verbose
        ; match_timeout
        ; number_of_workers
        ; dump_statistics
        ; newline_separate_rewrites
        }
    ; output_printer
    }
  =
  let number_of_workers = if sequential then 0 else number_of_workers in
  let scheduler = Scheduler.create ~number_of_workers () in
  let match_configuration = Configuration.create ~match_kind:Fuzzy () in
  let total_time = Statistics.Time.start () in

  let run_on_specifications input output_file =
    let result, count =
      List.fold specifications ~init:(Nothing,0) ~f:(fun (result, count) specification ->
          let input =
            match result with
            | Nothing | Matches _ -> input
            | Replacement (_, content, _) -> `String content
          in
          process_single_source matcher newline_separate_rewrites match_configuration input specification verbose match_timeout
          |> function
          | Nothing -> Nothing, count
          | Matches (x, number_of_matches) ->
            Matches (x, number_of_matches), count + number_of_matches
          | Replacement (x, content, number_of_matches) ->
            Replacement (x, content, number_of_matches),
            count + number_of_matches)
    in
    output_result output_printer output_file input result;
    count
  in

  match sources with
  | `String source ->
    let number_of_matches = run_on_specifications (`String source) None in
    (* FIXME(RVT): statistics for single source text doesn't output LOC *)
    write_statistics number_of_matches [] total_time dump_statistics
  | `Paths paths ->
    if sequential then
      let number_of_matches =
        List.fold ~init:0 paths ~f:(fun acc path ->
            let matches = run_on_specifications (`Path path) (Some path) in
            acc + matches)
      in
      write_statistics number_of_matches paths total_time dump_statistics
    else
      let map init paths =
        List.fold
          paths
          ~init
          ~f:(fun count path -> count + run_on_specifications (`Path path) (Some path))
      in
      let number_of_matches =
        try Scheduler.map_reduce scheduler ~init:0 ~map ~reduce:(+) paths
        with End_of_file -> 0
      in
      begin
        try Scheduler.destroy scheduler
        with Unix.Unix_error (_,"kill",_) ->
          (* No kill command on Mac OS X *)
          ()
      end;
      write_statistics number_of_matches paths total_time dump_statistics
  | `Zip zip_file ->
    if sequential then
      let zip_in = Zip.open_in zip_file in
      let not_in_an_exclude_directory filename =
        not (String.is_prefix ~prefix:exclude_directory_prefix filename)
      in
      let entries =
        match file_filters with
        | Some [] | None -> List.filter (Zip.entries zip_in) ~f:(fun { is_directory; filename; _ } ->
            not is_directory && not_in_an_exclude_directory filename)
        | Some suffixes ->
          let has_acceptable_suffix filename =
            List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename)
          in
          let not_in_an_exclude_directory filename =
            not (String.is_prefix ~prefix:exclude_directory_prefix filename)
          in
          List.filter (Zip.entries zip_in) ~f:(fun { is_directory; filename; _ } ->
              not is_directory && not_in_an_exclude_directory filename && has_acceptable_suffix filename)
      in
      let number_of_matches =
        List.fold ~init:0 entries ~f:(fun acc ({ filename; _ } as entry) ->
            let source = Zip.read_entry zip_in entry in
            let matches = run_on_specifications (`String source) (Some filename) in
            acc + matches)
      in
      Zip.close_in zip_in;
      write_statistics number_of_matches [] total_time dump_statistics
    else
      let map init (paths : Zip.entry list) =
        let zip_in = Zip.open_in zip_file in
        let result =
          List.fold
            paths
            ~init
            ~f:(fun count ({ filename; _ } as entry) ->
                let source = Zip.read_entry zip_in entry in
                let matches = run_on_specifications (`String source) (Some filename) in
                count + matches)
        in
        Zip.close_in zip_in;
        result
      in
      let number_of_matches =
        let zip_in = Zip.open_in zip_file in
        let not_in_an_exclude_directory filename =
          not (String.is_prefix ~prefix:exclude_directory_prefix filename)
        in
        let entries =
          match file_filters with
          | Some [] | None -> List.filter (Zip.entries zip_in) ~f:(fun { is_directory; filename; _ } ->
              not is_directory && not_in_an_exclude_directory filename)
          | Some suffixes ->
            let has_acceptable_suffix filename =
              List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename)
            in
            List.filter (Zip.entries zip_in) ~f:(fun { is_directory; filename; _ } ->
                not is_directory && not_in_an_exclude_directory filename && has_acceptable_suffix filename)
        in
        Zip.close_in zip_in;
        try Scheduler.map_reduce scheduler ~init:0 ~map ~reduce:(+) entries
        with End_of_file -> 0
      in
      begin
        try Scheduler.destroy scheduler
        with Unix.Unix_error (_,"kill",_) ->
          (* No kill command on Mac OS X *)
          ()
      end;
      write_statistics number_of_matches [] total_time dump_statistics
  | _ -> failwith "No single path handled here"

let list_supported_languages_and_exit () =
  let list =
    List.map Matchers.all ~f:(fun (module M) ->
        let ext = List.hd_exn M.extensions in
        Format.sprintf " -matcher %-10s%-10s\n" ext M.name)
    |> String.concat
  in
  Format.printf "%-20s%-10s@." "Option" "Language";
  Format.printf "%s%!" list;
  exit 0


let base_command_parameters : (unit -> 'result) Command.Param.t =
  [%map_open
     (* flags. *)
    let sequential = flag "sequential" no_arg ~doc:"Run sequentially"
    and match_only = flag "match-only" no_arg ~doc:"Only perform matching (ignore rewrite templates)"
    and verbose = flag "verbose" no_arg ~doc:(Format.sprintf "Log to %s" verbose_out_file)
    and rule = flag "rule" (optional_with_default "where true" string) ~doc:"rule Apply rules to matches."
    and match_timeout = flag "timeout" (optional_with_default 3 int) ~doc:"seconds Set match timeout on a source. Default: 3 seconds"
    and target_directory = flag "directory" ~aliases:["d"; "r"; "recursive"] (optional_with_default (Sys.getcwd ()) string) ~doc:(Format.sprintf "path Run recursively on files in a directory. Default is current directory: %s" @@ Sys.getcwd ())
    and directory_depth = flag "depth" (optional int) ~doc:"n Depth to recursively descend into directories"
    and specification_directories = flag "templates" (optional (Arg_type.comma_separated string)) ~doc:"path CSV of directories containing templates"
    and file_filters = flag "extensions" ~aliases:["e"; "file-extensions"; "f"] (optional (Arg_type.comma_separated string)) ~doc:"extensions Comma-separated extensions to include, like \".go\" or \".c,.h\". It is just a file suffix, so you can use it to filter file names like \"main.go\". The extension will be used to infer a matcher, unless --custom-matcher or --matcher is specified"
    and override_matcher = flag "matcher" ~aliases:["m"] (optional string) ~doc:"extension Use this matcher on all files regardless of their file extension, unless a --custom-matcher is specified"
    and custom_matcher = flag "custom-matcher" (optional string) ~doc:"path Path to a JSON file that contains a custom matcher"
    and zip_file = flag "zip" ~aliases:["z"] (optional string) ~doc:"zipfile A zip file containing files to rewrite"
    and json_pretty = flag "json-pretty" no_arg ~doc:"Output pretty JSON format"
    and json_lines = flag "json-lines" no_arg ~doc:"Output JSON line format"
    and json_only_diff = flag "json-only-diff" no_arg ~doc:"Output only the URI and diff in JSON line format"
    and file_in_place = flag "in-place" no_arg ~doc:"Rewrite files on disk, in place"
    and number_of_workers = flag "jobs" (optional_with_default 4 int) ~doc:"n Number of worker processes. Default: 4"
    and dump_statistics = flag "statistics" ~aliases:["stats"] no_arg ~doc:"Dump statistics to stderr"
    and stdin = flag "stdin" no_arg ~doc:"Read source from stdin"
    and stdout = flag "stdout" no_arg ~doc:"Print changed content to stdout. Useful to editors for reading in changed content."
    and diff = flag "diff" no_arg ~doc:"Output diff"
    and color = flag "color" no_arg ~doc:"Color matches or replacements (patience diff)."
    and newline_separated = flag "newline-separated" no_arg ~doc:"Instead of rewriting in place, output rewrites separated by newlines."
    and count = flag "count" no_arg ~doc:"Display a count of matches in a file."
    and list = flag "list" no_arg ~doc:"Display supported languages and extensions"
    and exclude_directory_prefix = flag "exclude-dir" (optional_with_default "." string) ~doc:"prefix of directories to exclude. Default: '.'"
    and anonymous_arguments =
      anon
        (maybe
           (t3
              ("MATCH_TEMPLATE" %: string)
              ("REWRITE_TEMPLATE" %: string)
              (sequence ("FULL_FILE_PATHS_OR_FILE_SUFFIXES" %: string))
           )
        )
    in
    let anonymous_arguments =
      Option.map anonymous_arguments ~f:(fun (match_template, rewrite_template, file_filters) ->
          let file_filters =
            match file_filters with
            | [] -> None
            | l ->
              List.map l ~f:(fun pattern ->
                  if String.contains pattern '/' then
                    match Filename.realpath pattern with
                    | exception Unix.Unix_error _ ->
                      Format.eprintf
                        "No such file or directory: %s. Comby interprets \
                         patterns containing '/' as file paths. If a pattern \
                         does not contain '/' (like '.ml'), it is considered a \
                         pattern where file endings must match the pattern. \
                         Please supply only valid file paths or patterns.@." pattern;
                      exit 1
                    | path -> path
                  else
                    pattern)
              |> Option.some
          in
          { match_template; rewrite_template; file_filters })
    in
    if list then list_supported_languages_and_exit ();
    let newline_separate_rewrites = newline_separated in
    let configuration =
      Command_configuration.create
        { input_options =
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
            }
        ; run_options =
            { sequential
            ; verbose
            ; match_timeout
            ; number_of_workers
            ; dump_statistics
            ; newline_separate_rewrites
            }
        ; output_options =
            { color
            ; count
            ; json_pretty
            ; json_lines
            ; json_only_diff
            ; file_in_place
            ; diff
            ; stdout
            ; newline_separated
            }
        }
      |> function
      | Ok configuration -> configuration
      | Error error ->
        Format.eprintf "%s@." @@ Error.to_string_hum error;
        exit 1
    in
    let (module M) as matcher, extension = select_matcher custom_matcher override_matcher configuration in
    fun () ->
      run matcher configuration;
      match extension with
      | Some ".generic" ->
        Format.eprintf "@.WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.@."
      | Some extension ->
        if M.name = "Generic" then
          Format.eprintf "@.WARNING: the GENERIC matcher was used because I'm unable to guess what language to use for the file extension %s. The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.@." extension
        else if debug then Format.eprintf "@.NOTE: the %s matcher was inferred from extension %s. See '-list' to set a matcher for a specific language.@." M.name extension
      | None -> ()
  ]

let default_command =
  Command.basic ~summary:"Run a rewrite pass." base_command_parameters

let () =
  Scheduler.Daemon.check_entry_point ();
  Command.run default_command ~version:"0.8.0"
