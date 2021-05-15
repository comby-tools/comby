open Core

open Comby_kernel

open Configuration
open Command_configuration
open Command_input
open Statistics

open Matchers

let verbose_out_file = "/tmp/comby.out"

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let timed_run
    (module Matcher : Matcher.S)
    ?(fast_offset_conversion = false)
    ~configuration
    ~source
    ~specification:(Specification.{ match_template = template; rule; rewrite_template })
    () =
  (match rewrite_template with
   | Some template -> Matcher.set_rewrite_template template;
   | None -> ());
  Matcher.all ?rule ~configuration ~template ~source ()
  |> List.map ~f:(Match.convert_offset ~fast:fast_offset_conversion ~source)

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
    ?(fast_offset_conversion = false)
    ?(verbose = false)
    ?(timeout = 3)
    ?metasyntax
    ?fresh
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
        match Rewrite.all ~source:input_text ?metasyntax ?fresh ~rewrite_template matches with
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

type run_mode =
  | Interactive
  | Command_line of
      { output_printer : Printer.t
      ; output_path : string option
      }

let run_on_specifications mode specifications process (input : single_source) =
  let result =
    List.fold specifications ~init:Nothing ~f:(fun result specification ->
        let input =
          match result with
          | Nothing
          | Matches _ -> input
          | Replacement (_, content, _) -> String content
        in
        match result, process input specification with
        | any, Nothing
        | Nothing, any -> any

        | Matches (l, n), Matches (l', n')  ->
          Matches (l@l', n+n')

        | Replacement (l, _, n), Replacement (l', content, n') ->
          Replacement (l@l', content, n+n')

        | Matches _, Replacement (l, content, n)
        | Replacement (l, content, n), Matches _ ->
          Format.eprintf
            "WARNING: input configuration specifies both rewrite \
             and match templates. I am choosing to only process the \
             configurations with both a 'match' and 'rewrite' part. \
             If you only want to see matches, add -match-only to \
             suppress this warning@.";
          Replacement (l, content, n)
      )
  in
  let count =
    match result with
    | Nothing -> 0
    | Matches (_, n)
    | Replacement (_, _, n) -> n
  in
  match mode with
  | Command_line { output_printer; output_path } ->
    output_result output_printer output_path input result;
    None, count
  | Interactive ->
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
    matcher
    fast_offset_conversion
    match_configuration
    verbose
    timeout
    sources
    compute_mode
    interactive_review =
  let with_rewrites ~input ~path:_ =
    run_on_specifications
      Interactive
      specifications
      (fun (input : single_source) specification ->
         process_single_source
           matcher
           ~fast_offset_conversion
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
        ; disable_substring_matching
        ; fast_offset_conversion
        ; match_newline_toplevel
        ; bound_count
        ; compute_mode
        }
    ; output_printer
    ; interactive_review
    ; metasyntax
    }
  =
  let fresh = match compute_mode with
    | `Sequential -> None
    | _ -> Some (fun () -> Uuid_unix.(Fn.compose Uuid.to_string create ()))
  in

  let match_configuration =
    Matchers.Configuration.create
      ~disable_substring_matching
      ~match_kind:Fuzzy
      ~match_newline_toplevel
      ?fresh
      ()
  in
  let start_time = Statistics.Time.start () in

  let per_unit ~(input : single_source) ~output_path =
    run_on_specifications
      (Command_line { output_printer; output_path })
      specifications
      (fun input specification ->
         process_single_source
           matcher
           ~fast_offset_conversion
           ~verbose
           ~timeout
           ?metasyntax
           ?fresh
           match_configuration
           input
           specification)
      input
    |> snd (* only count result for Command_line *)
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
        matcher
        fast_offset_conversion
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
    ?timeout
    ?metasyntax
    ?fresh
    ?(configuration = Matchers.Configuration.create ())
    source
    specification =
  process_single_source
    matcher
    ~fast_offset_conversion:false
    ~verbose:false
    ?timeout
    ?metasyntax
    ?fresh
    configuration
    source
    specification
