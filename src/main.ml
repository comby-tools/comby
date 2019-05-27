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
  | Rewritten of (Rewrite.match_context_replacement list * string * int)
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
              Some (Some (Rewrite.{ rewritten_source = input_text; in_place_substitutions = [] }), [])
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
        Rewritten (in_place_substitutions, rewritten_source, List.length matches)
      | Some (None, _)
      | None -> Nothing
  with
  | _ ->
    Nothing

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

let output_result output_printer output_options source_path source_content result =
  let source_content =
    match source_content with
    | `String content -> content
    | `Path path -> In_channel.read_all path
  in
  match result with
  | Nothing -> ()
  | Matches (matches, _) ->
    begin match output_printer with
      | Printer.Match_printer f -> f source_path matches
      | _ -> ()
    end
  | Rewritten (replacements, result, _) ->
    match source_path, output_options with
    (* rewrite in place *)
    | Some path, { json_pretty = false; json_lines = false; stdin = false; in_place = true; _ } ->
      Out_channel.write_all path ~data:result
    (* stdin, not JSON *)
    | _, { json_pretty = false; json_lines = false; stdin = true; in_place = false; _ } ->
      Format.printf "%s%!" result
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
      Format.printf "%s%!" result
    | _ -> failwith "Unhandled."

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
    ; output_options
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
            | Rewritten (_, content, _) -> `String content
          in
          process_single_source matcher match_configuration input specification verbose match_timeout
          |> function
          | Nothing -> Nothing, count
          | Matches (x, number_of_matches) ->
            Matches (x, number_of_matches), count + number_of_matches
          | Rewritten (x, content, number_of_matches) ->
            Rewritten (x, content, number_of_matches),
            count + number_of_matches)
    in
    output_result output_printer output_options output_file input result;
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
          let suffixes = fake_glob_file_extensions suffixes in
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
            let suffixes = fake_glob_file_extensions suffixes in
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
    and target_directory = flag "directory" ~aliases:["d"; "recursive"] (optional_with_default "." string) ~doc:(Format.sprintf "path Run recursively on files in a directory. Default is current directory: %s" @@ Sys.getcwd ())
    and specification_directories = flag "templates" (optional (Arg_type.comma_separated string)) ~doc:"path CSV of directories containing templates"
    and file_extensions = flag "extensions" ~aliases:["e"; "file-extensions"; "f"] (optional (Arg_type.comma_separated string)) ~doc:"extensions CSV of extensions to include, like \".go\" or \".c,.h\""
    and zip_file = flag "zip" ~aliases:["z"] (optional string) ~doc:"zipfile A zip file containing files to rewrite"
    and json_pretty = flag "json-pretty" no_arg ~doc:"Output pretty JSON format"
    and json_lines = flag "json-lines" no_arg ~doc:"Output JSON line format"
    and in_place = flag "in-place" no_arg ~doc:"Rewrite files on disk, in place"
    and number_of_workers = flag "jobs" (optional_with_default 4 int) ~doc:"n Number of worker processes. Default: 4"
    and dump_statistics = flag "statistics" ~aliases:["stats"] no_arg ~doc:"Dump statistics to stderr"
    and stdin = flag "stdin" no_arg ~doc:"Read source from stdin"
    and output_diff = flag "diff" no_arg ~doc:"Output colored diff"
    and anonymous_arguments =
      anon
        (maybe
           (t3
              ("MATCH_TEMPLATE" %: string)
              ("REWRITE_TEMPLATE" %: string)
              (maybe (sequence ("COMMA_SEPARATED_FILE_EXTENSIONS" %: (Arg_type.comma_separated string))))
           )
        )
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
            { json_pretty
            ; json_lines
            ; in_place
            ; stdin
            ; output_diff
            }
        }
    in
    fun () ->
      let (module M : Matchers.Matcher) =
        match file_extensions with
        | None | Some [] -> (module Matchers.Generic)
        | Some (hd::_) ->
          match hd with
          | ".c" | ".h" | ".cc" | ".cpp" | ".hpp" -> (module Matchers.C)
          | ".clj" -> (module Matchers.Clojure)
          | ".css" -> (module Matchers.CSS)
          | ".dart" -> (module Matchers.Dart)
          | ".elm" -> (module Matchers.Elm)
          | ".erl" -> (module Matchers.Erlang)
          | ".ex" -> (module Matchers.Elixir)
          | ".html" | ".xml" -> (module Matchers.Html)
          | ".hs" -> (module Matchers.Haskell)
          | ".go" -> (module Matchers.Go)
          | ".java" -> (module Matchers.Java)
          | ".js" | ".ts" -> (module Matchers.Javascript)
          | ".ml" | ".mli" -> (module Matchers.OCaml)
          | ".php" -> (module Matchers.Php)
          | ".py" -> (module Matchers.Python)
          | ".rb" -> (module Matchers.Ruby)
          | ".rs" -> (module Matchers.Rust)
          | ".s" | ".asm" -> (module Matchers.Assembly)
          | ".scala" -> (module Matchers.Scala)
          | ".sql" -> (module Matchers.SQL)
          | ".sh" -> (module Matchers.Bash)
          | ".swift" -> (module Matchers.Swift)
          | ".tex" | ".bib" -> (module Matchers.Latex)
          | _ -> (module Matchers.Generic)
      in
      run (module M) (Or_error.ok_exn configuration)
  ]

let default_command =
  Command.basic ~summary:"Run a rewrite pass." base_command_parameters

let () =
  Scheduler.Daemon.check_entry_point ();
  default_command
  |> Command.run ~version:"0.x.0"
