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

let verbose_out_file = "/tmp/comby.out"

let get_matches (module Matcher : Matchers.Matcher) configuration match_template match_rule source =
  let rule = Rule.create match_rule |> Or_error.ok_exn in
  Matcher.all ~configuration ~template:match_template ~source
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule ~matcher:(module Matcher) environment))

let apply_rewrite_rule matcher rewrite_rule matches =
  let open Option in
  match rewrite_rule with
  | "" -> matches
  | rewrite_rule ->
    begin
      match Rule.create rewrite_rule with
      | Ok rule ->
        List.filter_map matches ~f:(fun ({ environment; _ } as match_) ->
            let sat, env = Rule.apply rule ~matcher environment in
            (if sat then env else None)
            >>| fun environment -> { match_ with environment })
      | Error _ -> []
    end

let rewrite rewrite_template _rewrite_rule source matches =
  Rewrite.all ~source ~rewrite_template matches

let process_single_source
    matcher
    match_configuration
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
    | { match_specification = { match_template; match_rule }
      ; rewrite_specification = None
      } ->
      let matches =
        try
          let f () = get_matches matcher match_configuration match_template match_rule input_text in
          Statistics.Time.time_out ~after:match_timeout f ();
        with Statistics.Time.Time_out ->
          Format.eprintf "Timeout for input: %s!@." (show_input_kind source);
          Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
              Out_channel.output_lines out_channel [Format.sprintf "TIMEOUT: %s@." (show_input_kind source) ]);
          []
      in
      Matches (matches, List.length matches)
    | { match_specification = { match_template; match_rule }
      ; rewrite_specification = Some { rewrite_template; rewrite_rule }
      } ->
      let result =
        try
          let f () =
            get_matches matcher match_configuration match_template match_rule input_text
            |> fun matches ->
            (* TODO(RVT): merge match and rewrite rule application. *)
            apply_rewrite_rule matcher rewrite_rule matches
            |> fun matches ->
            if matches = [] then
              (* If there are no matches, return the original source (for editor support). *)
              Some (Some (Replacement.{ rewritten_source = input_text; in_place_substitutions = [] }), [])
            else
              Some (rewrite rewrite_template rewrite_rule input_text matches, matches)
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
    Format.eprintf "%s%!"
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
          process_single_source matcher match_configuration input specification verbose match_timeout
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
      let entries =
        match file_extensions with
        | Some [] | None -> List.filter (Zip.entries zip_in) ~f:(fun { is_directory; _ } -> not is_directory)
        | Some suffixes ->
          List.filter (Zip.entries zip_in) ~f:(fun { is_directory; filename; _ } ->
              not is_directory && List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename))
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
        let entries =
          match file_extensions with
          | Some [] | None -> List.filter (Zip.entries zip_in) ~f:(fun { is_directory; _ } -> not is_directory)
          | Some suffixes ->
            List.filter (Zip.entries zip_in) ~f:(fun { is_directory; filename; _ } ->
                not is_directory && List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename))
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
    and file_extensions = flag "extensions" ~aliases:["e"; "file-extensions"; "f"] (optional (Arg_type.comma_separated string)) ~doc:"extensions Comma-separated extensions to include, like \".go\" or \".c,.h\". It is just a file suffix, so you can use it to match whole file names like \"main.go\""
    and override_matcher = flag "matcher" ~aliases:["m"] (optional string) ~doc:"extension Use this matcher on all files regardless of their file extension"
    and zip_file = flag "zip" ~aliases:["z"] (optional string) ~doc:"zipfile A zip file containing files to rewrite"
    and json_pretty = flag "json-pretty" no_arg ~doc:"Output pretty JSON format"
    and json_lines = flag "json-lines" no_arg ~doc:"Output JSON line format"
    and json_only_diff = flag "json-only-diff" no_arg ~doc:"Output only the URI and diff in JSON line format"
    and in_place = flag "in-place" no_arg ~doc:"Rewrite files on disk, in place"
    and number_of_workers = flag "jobs" (optional_with_default 4 int) ~doc:"n Number of worker processes. Default: 4"
    and dump_statistics = flag "statistics" ~aliases:["stats"] no_arg ~doc:"Dump statistics to stderr"
    and stdin = flag "stdin" no_arg ~doc:"Read source from stdin"
    and stdout = flag "stdout" no_arg ~doc:"Print changed content to stdout. This option basically exists so editors can read in changed content."
    and diff = flag "diff" no_arg ~doc:"Output diff"
    and color = flag "color" no_arg ~doc:"Color matches or replacements (patience diff)."
    and count = flag "count" no_arg ~doc:"Display a count of matches in a file."
    and exclude_directory_prefix = flag "exclude-dir" (optional_with_default "." string) ~doc:"prefix of directories to exclude. Default: '.'"
    and anonymous_arguments =
      anon
        (maybe
           (t3
              ("MATCH_TEMPLATE" %: string)
              ("REWRITE_TEMPLATE" %: string)
              (maybe ("COMMA_SEPARATED_FILE_EXTENSIONS" %: (Arg_type.comma_separated string)))
           )
        )
    in
    let anonymous_arguments =
      Option.map anonymous_arguments ~f:(fun (match_template, rewrite_template, extensions) ->
          { match_template; rewrite_template; extensions })
    in
    let configuration =
      Command_configuration.create
        { input_options =
            { rule
            ; specification_directories
            ; anonymous_arguments
            ; file_extensions
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
            }
        ; output_options =
            { color
            ; count
            ; json_pretty
            ; json_lines
            ; json_only_diff
            ; in_place
            ; diff
            ; stdout
            }
        }
      |> function
      | Ok configuration -> configuration
      | Error error ->
        Format.eprintf "%s@." @@ Error.to_string_hum error;
        exit 1
    in
    fun () ->
      let matcher =
        if Option.is_some override_matcher then
          Matchers.select_with_extension (Option.value_exn override_matcher)
        else
          match file_extensions with
          | None | Some [] -> Matchers.select_with_extension ".generic"
          | Some (extension::_) -> Matchers.select_with_extension extension
      in
      run matcher configuration
  ]

let default_command =
  Command.basic ~summary:"Run a rewrite pass." base_command_parameters

let () =
  Scheduler.Daemon.check_entry_point ();
  default_command
  |> Command.run ~version:"0.5.1"
