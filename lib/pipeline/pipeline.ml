open Core

open Hack_parallel

open Configuration
open Command_configuration
open Command_input
open Rewriter
open Statistics

open Matchers
open Match
open Language

module Command_configuration = Command_configuration

let infer_equality_constraints environment =
  let vars = Environment.vars environment in
  List.fold vars ~init:[] ~f:(fun acc var ->
      if String.is_suffix var ~suffix:"_equal" then
        match String.split var ~on:'_' with
        | _uuid :: target :: _equal ->
          let expression = Language.Ast.Equal (Variable var, Variable target) in
          expression::acc
        | _ -> acc
      else
        acc)

let apply_rule ?(substitute_in_place = true) matcher rule matches =
  let open Option in
  List.filter_map matches ~f:(fun ({ environment; _ } as matched) ->
      let rule = rule @ infer_equality_constraints environment in
      let sat, env =  Rule.apply ~substitute_in_place ~matcher rule environment in
      (if sat then env else None)
      >>| fun environment -> { matched with environment })

let compute_line_col source offset =
  let f (offset, line, col) char =
    match offset, char with
    | 0, _ -> (0, line, col)
    | _, '\n' -> (offset - 1, line + 1, 1)
    | _ -> (offset - 1, line, col + 1)
  in
  let _, line, col = String.fold ~init:(offset, 1, 1) ~f source in
  line, col

let update_match source m =
  let update_range range =
    let update_location loc =
      let open Location in
      let line, column = compute_line_col source loc.offset in
      { loc with line; column}
    in
    let open Range in
    let match_start = update_location range.match_start in
    let match_end = update_location range.match_end in
    { match_start; match_end }
  in
  let update_environment env =
    List.fold (Environment.vars env) ~init:env ~f:(fun env var ->
        let open Option in
        let updated =
          Environment.lookup_range env var
          >>| update_range
          >>| Environment.update_range env var
        in
        Option.value_exn updated)
  in
  let range = update_range m.range in
  let environment = update_environment m.environment in
  { m with range; environment }

let timed_run matcher ?substitute_in_place ?rule ~configuration ~template ~source () =
  let module Matcher = (val matcher : Matchers.Matcher) in
  let matches = Matcher.all ~configuration ~template ~source in
  let rule = Option.value rule ~default:[Ast.True] in
  apply_rule ?substitute_in_place matcher rule matches
  |> List.map ~f:(update_match source)

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

type processed_source_result =
  | Matches of (Match.t list * int)
  | Replacement of (Replacement.t list * string * int)
  | Nothing

let verbose_out_file = "/tmp/comby.out"

let with_timeout timeout source ~f =
  try Statistics.Time.time_out ~after:timeout f ();
  with Statistics.Time.Time_out ->
    Format.eprintf "Timeout for input: %s!@." (show_input_kind source);
    Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
        Out_channel.output_lines out_channel [Format.sprintf "TIMEOUT: %s@." (show_input_kind source) ]);
    []

let log_to_file path =
  Out_channel.with_file ~append:true verbose_out_file ~f:(fun out_channel ->
      Out_channel.output_lines out_channel [Format.sprintf "Processing %s%!" path])

let process_single_source
    matcher
    substitute_in_place
    configuration
    source
    Specification.{ match_template = template; rule ; rewrite_template }
    verbose
    timeout =
  try
    let input_text =
      match source with
      | `String input_text -> input_text
      | `Path path ->
        if verbose then log_to_file path;
        In_channel.read_all path
    in
    let matches =
      with_timeout timeout source ~f:(fun () ->
          timed_run matcher ?rule ~substitute_in_place ~configuration ~template ~source:input_text ())
    in
    match rewrite_template with
    | None -> Matches (matches, List.length matches)
    | Some rewrite_template ->
      match matches with
      | [] ->
        (* If there are no matches, return the original source (for editor support). *)
        Replacement ([], input_text, 0)
      | matches ->
        match Rewrite.all ~source:input_text ~rewrite_template matches with
        | None -> Nothing
        | Some { rewritten_source; in_place_substitutions } ->
          Replacement (in_place_substitutions, rewritten_source, List.length matches)
  with
  | exn ->
    if debug then Format.eprintf "Big error: %s@." (Exn.to_string exn);
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

let with_zip zip_file ~f =
  let zip_in = Zip.open_in zip_file in
  let result = f zip_in in
  Zip.close_in zip_in;
  result

let run_on_specifications specifications output_printer process input output_file =
  let result, count =
    List.fold specifications ~init:(Nothing, 0) ~f:(fun (result, count) specification ->
        let input =
          match result with
          | Nothing | Matches _ -> input
          | Replacement (_, content, _) -> `String content
        in
        process input specification
        |> function
        | Nothing -> Nothing, count
        | Matches (m, number_of_matches) ->
          Matches (m, number_of_matches), count + number_of_matches
        | Replacement (r, content, number_of_matches) ->
          Replacement (r, content, number_of_matches),
          count + number_of_matches)
  in
  output_result output_printer output_file input result;
  count

let run_on_specifications_with_rewrites specifications process input =
  let result, count =
    List.fold specifications ~init:(Nothing, 0) ~f:(fun (result, count) specification ->
        let input =
          match result with
          | Nothing | Matches _ -> input
          | Replacement (_, content, _) -> `String content
        in
        process input specification
        |> function
        | Nothing -> Nothing, count
        | Matches (m, number_of_matches) ->
          Matches (m, number_of_matches), count + number_of_matches
        | Replacement (r, content, number_of_matches) ->
          if number_of_matches = 0 then
            Nothing, count
          else
            Replacement (r, content, number_of_matches),
            count + number_of_matches)
  in
  match result with
  | Replacement (_, content, _) -> Some content, count
  | _ -> None, 0

let with_scheduler scheduler ~f =
  let result = f scheduler in
  begin
    try Scheduler.destroy scheduler
    with Unix.Unix_error (_,"kill",_) -> Format.printf "UH OH@."; ()
  end;
  result

let try_or_skip f scheduler ~default =
  try f scheduler with End_of_file -> default

let map_reduce ~init ~map ~reduce data scheduler =
  Scheduler.map_reduce scheduler ~init ~map ~reduce data

let process_paths ~sequential ~f paths scheduler =
  let process_bucket ~init paths =
    List.fold ~init paths ~f:(fun count path ->
        count + f ~input:(`Path path) ~path:(Some path))
  in
  if sequential then process_bucket ~init:0 paths
  else
    let map acc bucket_of_paths = process_bucket ~init:acc bucket_of_paths in
    let reduce = (+) in
    let init = 0 in
    let f = map_reduce ~init ~map ~reduce paths in
    with_scheduler scheduler ~f:(try_or_skip f ~default:0)

let process_paths_for_interactive ~sequential ~f paths scheduler =
  let process_bucket ~init paths =
    List.fold ~init paths ~f:(fun (acc,c) path ->
        match f ~input:(`Path path) ~path:(Some path) with
        | Some rewritten_source, c' -> Interactive.{ path; rewritten_source}::acc,c+c'
        | None, c' -> acc,c+c')
  in
  if sequential then process_bucket ~init:([],0) paths
  else
    let map acc bucket_of_paths = process_bucket ~init:acc bucket_of_paths in
    let reduce (acc', c') (acc,c) = (List.append acc acc'), (c' + c) in
    let init = ([],0) in
    let f = map_reduce ~init ~map ~reduce paths in
    with_scheduler scheduler ~f:(try_or_skip f ~default:([],0))

let filter_zip_entries file_filters exclude_directory_prefix zip =
  let not_in_an_exclude_directory prefix filename = not (String.is_prefix ~prefix filename) in
  match file_filters with
  | Some [] | None -> List.filter (Zip.entries zip) ~f:(fun { is_directory; filename; _ } ->
      not is_directory && not_in_an_exclude_directory exclude_directory_prefix filename)
  | Some suffixes ->
    let has_acceptable_suffix filename =
      List.exists suffixes ~f:(fun suffix -> String.is_suffix ~suffix filename)
    in
    List.filter (Zip.entries zip) ~f:(fun { is_directory; filename; _ } ->
        not is_directory
        && not_in_an_exclude_directory exclude_directory_prefix filename
        && has_acceptable_suffix filename)

let process_zip_file ~sequential ~f scheduler zip_file exclude_directory_prefix file_filters =
  let process_zip_bucket ~init (paths : Zip.entry list) zip =
    List.fold ~init paths ~f:(fun count ({ filename; _ } as entry) ->
        let source = Zip.read_entry zip entry in
        count + f ~input:(`String source) ~path:(Some filename))
  in
  let process_bucket ~init paths = with_zip zip_file ~f:(process_zip_bucket ~init paths) in
  let paths = with_zip zip_file ~f:(filter_zip_entries file_filters exclude_directory_prefix) in
  if sequential then process_bucket ~init:0 paths
  else
    let map acc bucket_of_paths = process_bucket ~init:acc bucket_of_paths in
    let reduce = (+) in
    let init = 0 in
    let f = map_reduce ~init ~map ~reduce paths in
    with_scheduler scheduler ~f:(try_or_skip f ~default:0)

let write_statistics file_filters exclude_directory_prefix number_of_matches sources start_time =
  let total_time = Statistics.Time.stop start_time in
  let lines_of_code, number_of_files =
    match sources with
    | `String source ->
      List.length (String.split_lines source), 1
    | `Paths paths ->
      let lines_of_code =
        List.fold paths ~init:0 ~f:(fun acc paths ->
            In_channel.read_lines paths
            |> List.length
            |> (+) acc)
      in
      lines_of_code, List.length paths
    | `Zip zip_file ->
      let paths = with_zip zip_file ~f:(filter_zip_entries file_filters exclude_directory_prefix) in
      let lines_of_code =
        with_zip zip_file ~f:(fun zip ->
            List.fold paths ~init:0 ~f:(fun acc entry ->
                let source = Zip.read_entry zip entry in
                acc + (List.length (String.split_lines source))))in
      lines_of_code, List.length paths
    | _ -> failwith "No single path handled here"
  in
  let statistics =
    { number_of_files
    ; lines_of_code
    ; number_of_matches
    ; total_time = total_time
    }
  in
  Format.eprintf "%s@."
  @@ Yojson.Safe.pretty_to_string
  @@ Statistics.to_yojson statistics

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
        ; substitute_in_place
        ; disable_substring_matching
        }
    ; output_printer
    ; interactive_review
    }
  =
  let number_of_workers = if sequential then 0 else number_of_workers in
  let scheduler = Scheduler.create ~number_of_workers () in
  let match_configuration = Configuration.create ~disable_substring_matching ~match_kind:Fuzzy () in
  let start_time = Statistics.Time.start () in

  let per_unit ~input ~path =
    run_on_specifications
      specifications
      output_printer
      (fun input specification ->
         process_single_source
           matcher
           substitute_in_place
           match_configuration
           input
           specification
           verbose
           match_timeout)
      input
      path
  in
  let count =
    if Option.is_none interactive_review then
      match sources with
      | `String source -> with_scheduler scheduler ~f:(fun _ -> per_unit ~input:(`String source) ~path:None)
      | `Paths paths -> process_paths ~sequential ~f:per_unit paths scheduler
      | `Zip zip_file -> process_zip_file ~sequential ~f:per_unit scheduler zip_file exclude_directory_prefix file_filters
      | _ -> failwith "No single path handled here"
    else
      let rewrites, count =
        with_scheduler scheduler ~f:(fun _scheduler ->
            match sources with
            | `Paths paths ->
              let with_rewrites ~input ~path:_ =
                run_on_specifications_with_rewrites
                  specifications
                  (fun input specification ->
                     process_single_source
                       matcher
                       substitute_in_place
                       match_configuration
                       input
                       specification
                       verbose
                       match_timeout)
                  input
              in
              let count, inputs = process_paths_for_interactive ~sequential ~f:with_rewrites paths scheduler in
              count, inputs
            | _ -> failwith "Can't run interactive review for these inputs")
      in
      let { editor; default_is_accept } = Option.value_exn interactive_review in
      Interactive.run editor default_is_accept count rewrites;
      count
  in
  if dump_statistics then write_statistics file_filters exclude_directory_prefix count sources start_time;
