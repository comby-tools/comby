open Core

open Configuration
open Command_configuration
open Command_input
open Rewriter
open Statistics

open Match
open Language

let verbose_out_file = "/tmp/comby.out"

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

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

let apply_rule ?(substitute_in_place = true) matcher omega rule matches =
  let open Option in
  List.filter_map matches ~f:(fun ({ environment; _ } as matched) ->
      let rule = rule @ infer_equality_constraints environment in
      let apply =
        if omega then
          Rule.Omega.apply
        else
          Rule.Alpha.apply
      in
      let sat, env = apply ~substitute_in_place ~matcher rule environment in
      (if sat then env else None)
      >>| fun environment -> { matched with environment })

let timed_run
    (module Matcher : Matchers.Matcher.S)
    ?(fast_offset_conversion = false)
    ?(omega = false)
    ?substitute_in_place
    ~configuration
    ~source
    ~specification:(Specification.{ match_template = template; rule; rewrite_template })
    () =
  (match rewrite_template with
   | Some template -> Matcher.set_rewrite_template template;
   | None -> ());
  let rule = Option.value rule ~default:[Ast.True] in
  let options = if omega then Rule.Omega.options rule else Rule.Alpha.options rule in
  let matches = Matcher.all ~nested:options.nested ~configuration ~template ~source () in
  let matches = apply_rule ?substitute_in_place (module Matcher) omega rule matches in
  List.map matches ~f:(Match.convert_offset ~fast:fast_offset_conversion ~source)

type output =
  | Matches of (Match.t list * int)
  | Replacement of (Replacement.t list * string * int)
  | Nothing

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
    ?(sequential = false)
    ?(omega = false)
    ?(fast_offset_conversion = false)
    ?(substitute_in_place = false)
    ?(verbose = false)
    ?(timeout = 3)
    ?metasyntax
    configuration
    source
    (Specification.{ rewrite_template; _ } as specification)
  =
  try
    let input_text =
      match source with
      | String input_text -> input_text
      | Path path ->
        if verbose then log_to_file path;
        In_channel.read_all path
    in
    let matches =
      with_timeout timeout source ~f:(fun () ->
          timed_run
            matcher
            ~substitute_in_place
            ~omega
            ~fast_offset_conversion
            ~configuration
            ~specification
            ~source:input_text
            ())
    in
    match rewrite_template with
    | None -> Matches (matches, List.length matches)
    | Some rewrite_template ->
      match matches with
      | [] ->
        (* If there are no matches, return the original source (for editor support). *)
        Replacement ([], input_text, 0)
      | matches ->
        match Rewrite.all ~source:input_text ?metasyntax ~sequential ~rewrite_template matches with
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
      | String content -> content
      | Path path -> In_channel.read_all path
    in
    output_printer (Printer.Replacements { source_path; replacements; result; source_content })

let run_on_specifications specifications output_printer process (input : single_source) output_file =
  let result, count =
    List.fold specifications ~init:(Nothing, 0) ~f:(fun (result, count) specification ->
        let input =
          match result with
          | Nothing | Matches _ -> input
          | Replacement (_, content, _) -> String content
        in
        process input specification
        |> function
        | Nothing -> Nothing, count
        | Matches (l, number_of_matches) ->
          Matches (l, number_of_matches), count + number_of_matches
        | Replacement (l, content, number_of_matches) ->
          Replacement (l, content, number_of_matches),
          count + number_of_matches)
  in
  output_result output_printer output_file input result;
  count

let run_on_specifications_for_interactive specifications process (input : single_source) =
  let result, count =
    List.fold specifications ~init:(Nothing, 0) ~f:(fun (result, count) specification ->
        let input =
          match result with
          | Nothing | Matches _ -> input
          | Replacement (_, content, _) -> String content
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

let write_statistics number_of_matches sources start_time =
  let total_time = Statistics.Time.stop start_time in
  let lines_of_code, number_of_files =
    match sources with
    | `String source ->
      List.length (String.split_lines source), 1
    | `Paths paths ->
      let lines_of_code = Fold.loc_paths paths in
      lines_of_code, List.length paths
    | `Zip (zip_file, paths) ->
      let lines_of_code = Fold.loc_zip zip_file paths in
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

let run_batch ~f:per_unit sources compute_mode bound_count =
  match compute_mode with
  | `Sequential ->
    Sequential.process ~f:per_unit bound_count sources
  | `Parany number_of_workers ->
    Parallel_parany.process ~f:per_unit number_of_workers bound_count sources
  | `Hack_parallel number_of_workers ->
    Parallel_hack.process ~f:per_unit number_of_workers bound_count sources

let run_interactive
    specifications
    sequential
    matcher
    omega
    fast_offset_conversion
    substitute_in_place
    match_configuration
    verbose
    timeout
    sources
    compute_mode
    interactive_review =
  let with_rewrites ~input ~path:_ =
    run_on_specifications_for_interactive
      specifications
      (fun (input : single_source) specification ->
         process_single_source
           matcher
           ~sequential
           ~omega
           ~fast_offset_conversion
           ~substitute_in_place
           ~verbose
           ~timeout
           match_configuration
           input
           specification)
      input
  in
  let paths =
    match sources with
    | `Paths paths -> paths
    | _ -> failwith "Cannot run interactive mode with this input source, must be file paths."
  in
  let rewrites, count =
    match compute_mode with
    | `Sequential -> Sequential.process_interactive ~f:with_rewrites paths
    | `Parany number_of_workers -> Parallel_parany.process_interactive ~f:with_rewrites paths number_of_workers
    | `Hack_parallel number_of_workers -> Parallel_hack.process_interactive ~f:with_rewrites paths number_of_workers
  in
  let { editor; default_is_accept } = interactive_review in
  Interactive.run editor default_is_accept count rewrites;
  count

let run
    { matcher
    ; sources
    ; specifications
    ; run_options =
        { verbose
        ; match_timeout = timeout
        ; dump_statistics
        ; substitute_in_place
        ; disable_substring_matching
        ; omega
        ; fast_offset_conversion
        ; match_newline_toplevel
        ; bound_count
        ; compute_mode
        }
    ; output_printer
    ; interactive_review
    ; extension = _ (* FIXME *)
    ; metasyntax
    }
  =
  let sequential = match compute_mode with | `Sequential -> true | _ -> false in
  let match_configuration =
    Matchers.Configuration.create
      ~disable_substring_matching
      ~match_kind:Fuzzy
      ~match_newline_toplevel
      ()
  in
  let start_time = Statistics.Time.start () in

  let per_unit ~(input : single_source) ~output_path =
    run_on_specifications
      specifications
      output_printer
      (fun input specification ->
         process_single_source
           matcher
           ?metasyntax
           ~sequential
           ~omega
           ~fast_offset_conversion
           ~substitute_in_place
           ~verbose
           ~timeout
           match_configuration
           input
           specification)
      input
      output_path
  in
  let count =
    match interactive_review with
    | None ->
      begin match sources with
        | `String source ->  per_unit ~input:(String source) ~output_path:None
        | #batch_input as sources -> run_batch ~f:per_unit sources compute_mode bound_count
      end
    | Some interactive_review ->
      run_interactive
        specifications
        sequential
        matcher
        omega
        fast_offset_conversion
        substitute_in_place
        match_configuration
        verbose
        timeout
        sources
        compute_mode
        interactive_review
  in
  if dump_statistics then write_statistics count sources start_time

let execute
    matcher
    ?substitute_in_place
    ?timeout
    ?metasyntax
    ?(configuration = Matchers.Configuration.create ())
    source
    specification =
  process_single_source
    matcher
    ~sequential:true
    ~omega:false
    ~fast_offset_conversion:false
    ?substitute_in_place
    ~verbose:false
    ?timeout
    ?metasyntax
    configuration
    source
    specification
