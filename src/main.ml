open Core
open Command.Let_syntax

open Hack_parallel

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

type input_kind =
  | Paths of string list
  | Path of string
  | String of string
[@@deriving show]

type processed_source_result =
  | Matches of (Match.t list * int)
  | Rewritten of (Rewrite.match_context_replacement list * string * int)
  | Nothing

let read = Fn.compose String.rstrip In_channel.read_all

let read_template =
  Fn.compose
    String.chop_suffix_exn ~suffix:"\n"
    In_channel.read_all

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

let process_single_source matcher verbose configuration source specification match_timeout =
  let open Specification in
  try
    let input_text =
      match source with
      | String input_text -> input_text
      | Path path ->
        if verbose then
          Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
              Out_channel.output_lines out_channel [Format.sprintf "Processing %s%!" path]);
        In_channel.read_all path
      | _ -> failwith "Don't send multiple paths to process_single_source"
    in
    match specification with
    | { match_specification = { match_template; match_rule }
      ; rewrite_specification = None
      } ->
      let matches =
        try
          let f () = get_matches matcher configuration match_template match_rule input_text in
          Statistics.Time.time_out ~after:match_timeout f ();
        with Statistics.Time.Time_out ->
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
            get_matches matcher configuration match_template match_rule input_text
            |> fun matches ->
            (* TODO(RVT): merge match and rewrite rule application. *)
            apply_rewrite_rule matcher rewrite_rule matches
            |> fun matches ->
            if matches = [] then
              (* If there are no matches, return the original source (for editor support). *)
              Some (Some (Rewrite.{ rewritten_source = input_text; contextual_substitutions = [] }), [])
            else
              Some (rewrite rewrite_template rewrite_rule input_text matches, matches)
          in
          Statistics.Time.time_out ~after:match_timeout f ();
        with Statistics.Time.Time_out ->
          Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
              Out_channel.output_lines out_channel [Format.sprintf "TIMEOUT: FOR %s@." (show_input_kind source) ]);
          None
      in
      result
      |> function
      | Some (Some { rewritten_source; contextual_substitutions }, matches) ->
        Rewritten (contextual_substitutions, rewritten_source, List.length matches)
      | Some (None, _)
      | None -> Nothing
  with
  | _ -> Nothing

let output_result stdin spec_number json source_path result =
  match result with
  | Nothing -> ()
  | Matches (matches, _) ->
    if json then
      let json_matches = `List (List.map ~f:Match.to_yojson matches) in
      Format.printf "%s%!" @@ Yojson.Safe.pretty_to_string json_matches
    else
      let with_file =
        match source_path with
        | Some path -> Format.sprintf " in %s " path
        | None -> " "
      in
      Format.printf
        "%d matches%sfor spec %d (add -json for json format)@."
        (List.length matches)
        with_file
        (spec_number + 1)
  | Rewritten (replacements, result, _) ->
    match source_path, json, stdin with
    (* default: rewrite in place *)
    | Some path, false, false -> Out_channel.write_all path ~data:result
    (* stdin, not JSON *)
    | _, false, true -> Format.printf "%s%!" result
    (* stdin, JSON with path *)
    | Some path, true, _ ->
      let json_rewrites =
        let value = `List (List.map ~f:Rewrite.match_context_replacement_to_yojson replacements) in
        `Assoc [(path, value)]
      in
      Format.printf "%s%!" @@ Yojson.Safe.pretty_to_string json_rewrites
    (* JSON, no path *)
    | None, true, _ ->
      let json_rewrites =
        `List (List.map ~f:Rewrite.match_context_replacement_to_yojson replacements) in
      Format.printf "%s%!" @@ Yojson.Safe.pretty_to_string json_rewrites
    | _ -> Format.printf "%s%!" result

let write_statistics number_of_matches paths total_time =
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
    (sources : input_kind)
    (specifications : Specification.t list)
    sequential
    number_of_workers
    stdin
    json
    verbose
    match_timeout =
  let number_of_workers = if sequential then 0 else number_of_workers in
  let scheduler = Scheduler.create ~number_of_workers () in
  let configuration = Configuration.create ~match_kind:Fuzzy () in
  let total_time = Statistics.Time.start () in

  let run_on_specifications input output_file =
    let result, count =
      List.fold specifications ~init:(Nothing,0) ~f:(fun (result, count) specification ->
          let input =
            match result with
            | Nothing | Matches _ -> input
            | Rewritten (_, content, _) -> String content
          in
          process_single_source matcher verbose configuration input specification match_timeout
          |> function
          | Nothing -> Nothing, count
          | Matches (x, number_of_matches) ->
            Matches (x, number_of_matches), count + number_of_matches
          | Rewritten (x, content, number_of_matches) ->
            Rewritten (x, content, number_of_matches),
            count + number_of_matches)
    in
    output_result stdin 0 json output_file result;
    count
  in

  match sources with
  | String source ->
    let number_of_matches = run_on_specifications (String source) None in
    (* FIXME(RVT): statistics for single source text doesn't output LOC *)
    write_statistics number_of_matches [] total_time
  | Paths paths ->
    if sequential then
      let number_of_matches =
        List.fold ~init:0 paths ~f:(fun acc path ->
            let matches = run_on_specifications (Path path) (Some path) in
            acc + matches)
      in
      write_statistics number_of_matches paths total_time
    else
      let map init paths =
        List.fold
          paths
          ~init
          ~f:(fun count path -> count + run_on_specifications (Path path) (Some path))
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
      write_statistics number_of_matches paths total_time
  | _ -> failwith "No single path handled here"

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

let base_command_parameters : (unit -> 'result) Command.Param.t =
  [%map_open
     (* flags. *)
    let sequential = flag "sequential" no_arg ~doc:"Run sequentially"
    and match_only = flag "match-only" no_arg ~doc:"Only perform matching (ignore rewrite templates)"
    and verbose = flag "verbose" no_arg ~doc:(Format.sprintf "Log to %s" verbose_out_file)
    and rule = flag "rule" (optional_with_default "where true" string) ~doc:"rule Apply rules to matches. Respects -f"
    and match_timeout = flag "timeout" (optional_with_default 3 int) ~doc:"seconds Set match timeout on a source. Default: 3"
    and target_directory = flag "directory" (optional_with_default "." string) ~doc:(Format.sprintf "path Run on files in a directory. Default is current directory: %s" @@ Sys.getcwd ())
    and specification_directories = flag "templates" (optional (Arg_type.comma_separated string)) ~doc:"path CSV of directories containing templates"
    and file_extensions = flag "filter" (optional (Arg_type.comma_separated string)) ~doc:"extensions CSV of extensions to include"
    and json = flag "json" no_arg ~doc:"Output JSON format for matches or rewrite text to stdout"
    and number_of_workers = flag "jobs" (optional_with_default 4 int) ~doc:"n Number of worker processes. Default: 4"
    and stdin = flag "stdin" no_arg ~doc:"Read source from stdin"
    and anonymous_arguments =
      anon (maybe (t2
                     ("MATCH_TEMPLATE" %: string)
                     ("REWRITE_TEMPLATE" %: string)))
    in
    fun () ->
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
            "Please either specify templates on the command line or using \
             -templates [dir] for templates in directory [dir].@.";
          exit 1
        | None, Some (match_template, rewrite_template) ->
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
      let sources =
        match stdin with
        | false -> Paths (parse_source_directories ?file_extensions target_directory)
        | true -> String (In_channel.input_all In_channel.stdin)
      in

      let matcher =
        let default = (module Matchers.C : Matchers.Matcher) in
        match file_extensions with
        | None | Some [] -> default
        | Some (hd::_) ->
          match hd with
          | ".c" | ".h" | ".cc" | ".cpp" | ".hpp" -> (module Matchers.C : Matchers.Matcher)
          | ".py" -> (module Matchers.Python : Matchers.Matcher)
          | ".go" -> (module Matchers.Go : Matchers.Matcher)
          | ".sh" -> (module Matchers.Bash : Matchers.Matcher)
          | ".html" -> (module Matchers.Html : Matchers.Matcher)
          | _ -> default
      in
      run matcher sources specifications sequential number_of_workers stdin json verbose match_timeout
  ]

let default_command =
  Command.basic ~summary:"Run a rewrite pass." base_command_parameters

let () =
  Scheduler.Daemon.check_entry_point ();
  default_command
  |> Command.run
