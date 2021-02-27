open! Core
open! Import

let lines_of_contents contents =
  let lines = Array.of_list (String.split_lines contents) in
  let has_trailing_newline =
    let length = String.length contents in
    if length = 0 || Char.equal contents.[length - 1] '\n'
    then `With_trailing_newline
    else `Missing_trailing_newline
  in
  lines, has_trailing_newline
;;

let%test_unit _ =
  let test contents ~expect =
    [%test_result: string array * [ `With_trailing_newline | `Missing_trailing_newline ]]
      (lines_of_contents contents)
      ~expect
  in
  test "" ~expect:([||], `With_trailing_newline);
  test "hello" ~expect:([| "hello" |], `Missing_trailing_newline);
  test "hello\nworld" ~expect:([| "hello"; "world" |], `Missing_trailing_newline);
  test "hello\nworld\n" ~expect:([| "hello"; "world" |], `With_trailing_newline)
;;

let convert_to_patch_compatible_hunks ?prev_diff ?next_diff lines_prev lines_next hunks =
  let open Diff_input in
  let open Patience_diff.Hunk in
  let open Patience_diff.Range in
  if Option.is_none prev_diff || Option.is_none next_diff then
    ()
  else
    let prev = Option.value_exn prev_diff in
    let next = Option.value_exn next_diff in
    let has_trailing_newline text =
      try String.contains ~pos:(String.length text - 1) text '\n' with _ -> false
    in
    let prev_file_no_trailing_newline = not (has_trailing_newline prev.text) in
    let next_file_no_trailing_newline = not (has_trailing_newline next.text) in
    if prev_file_no_trailing_newline || next_file_no_trailing_newline then
      (* if file does not have newline, and start + size - 1 = number of lines in file without a newline, then add \... *)
      List.iter hunks ~f:(fun hunk ->
          if
            Array.length lines_prev = (hunk.prev_start + hunk.prev_size - 1)
            || Array.length lines_next = (hunk.next_start + hunk.next_size - 1)
          then
            let correction = "\n\\ No newline at end of file" in
            (* We can't just take the last because the second last might be a prev
               that needs the message too (because for unrefined patdiff converts
               the replace to a prev and a next). We can't just handle all ranges
               in this hunk either, because some prev or next might be earlier
               than the rest of the prev or nexts. *)
            List.last hunk.ranges
            |> function
              (* if last hunk range are the same *)
              | Some Same a ->
                 let prev, next = Array.get a (Array.length a - 1) in
                 Array.set a (Array.length a - 1) (prev^correction, next^correction)
              (* if the last hunk had things deleted *)
              | Some Prev a when prev_file_no_trailing_newline ->
                 let prev = Array.get a (Array.length a - 1) in
                 Array.set a (Array.length a - 1) (prev^correction)
              | Some Next a when next_file_no_trailing_newline ->
                 (* Prev has a newline, dest does not *)
                 let next = Array.get a (Array.length a - 1) in
                 Array.set a (Array.length a - 1) (next^correction)
              | Some Unified a ->
                 let unified = Array.get a (Array.length a - 1) in
                 Array.set a (Array.length a - 1) (unified^correction)
              | Some Replace (a1, a2) ->
                 if prev_file_no_trailing_newline then
                   (
                     let prev = Array.get a1 (Array.length a1 - 1) in
                     Array.set a1 (Array.length a1 - 1) (prev^correction)
                   );
                 if next_file_no_trailing_newline then
                   (
                     let next = Array.get a2 (Array.length a2 - 1) in
                     Array.set a2 (Array.length a2 - 1) (next^correction)
                   )
              | _ -> ())
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_lines (config : Configuration.t) ?prev_diff ?next_diff ~prev ~next () =
  (* Create the diff *)
  let context = config.context in
  let keep_ws = config.keep_ws in
  let split_long_lines = config.split_long_lines in
  let line_big_enough = config.line_big_enough in
  let hunks =
    let transform = if keep_ws then Fn.id else Patdiff_core.remove_ws in
    (* Use external compare program? *)
    match config.ext_cmp with
    | None ->
      Patience_diff.String.get_hunks
        ~transform
        ~context
        ~big_enough:line_big_enough
        ~prev
        ~next
    | Some prog ->
      let compare x y =
        let cmd = sprintf "%s %S %S" prog x y in
        match Unix.system cmd with
        | Ok () -> 0
        | Error (`Exit_non_zero 1) -> 1
        | Error _ -> failwithf "External compare %S failed!" prog ()
      in
      let module P =
        Patience_diff.Make (struct
          type t = string [@@deriving sexp]

          let hash = String.hash
          let compare = compare
        end)
      in
      P.get_hunks ~transform ~context ~big_enough:line_big_enough ~prev ~next
  in
  let hunks =
    match config.float_tolerance with
    | None -> hunks
    | Some tolerance -> Float_tolerance.apply hunks tolerance ~context
  in
  (* Refine if desired *)
  if config.unrefined
  then
    (
      (* Before turning replace ranges into `Prev and `Next, add the diff fixups *)
      convert_to_patch_compatible_hunks ?prev_diff ?next_diff prev next hunks;
      (* Turn `Replace ranges into `Prev and `Next ranges.
         `Replace's would otherwise be later interpreted as refined output *)
      Patience_diff.Hunks.unified hunks
    )
  else (
    let rules = config.rules in
    let output = config.output in
    let produce_unified_lines = config.produce_unified_lines in
    let interleave = config.interleave in
    let word_big_enough = config.word_big_enough in
    Patdiff_core.refine
      ~rules
      ~output
      ~keep_ws
      ~produce_unified_lines
      ~split_long_lines
      ~interleave
      hunks
      ~word_big_enough)
;;

let warn_if_no_trailing_newline
      ~warn_if_no_trailing_newline_in_both
      (prev_file_newline, prev_file)
      (next_file_newline, next_file)
  =
  let warn = eprintf "No newline at the end of %s\n%!" in
  match prev_file_newline, next_file_newline with
  | `With_trailing_newline, `With_trailing_newline -> ()
  | `With_trailing_newline, `Missing_trailing_newline -> warn next_file
  | `Missing_trailing_newline, `With_trailing_newline -> warn prev_file
  | `Missing_trailing_newline, `Missing_trailing_newline ->
    if warn_if_no_trailing_newline_in_both
    then (
      warn prev_file;
      warn next_file)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_files (config : Configuration.t) ~prev_file ~next_file =
  let prev = In_channel.read_all (File_name.real_name_exn prev_file) in
  let next = In_channel.read_all (File_name.real_name_exn next_file) in
  Comparison_result.create
    config
    ~prev:{ name = File_name.display_name prev_file; text = prev }
    ~next:{ name = File_name.display_name next_file; text = next }
    ~compare_assuming_text:(fun config ~prev ~next ->
      let prev_lines, prev_file_newline = lines_of_contents prev.text in
      let next_lines, next_file_newline = lines_of_contents next.text in
      warn_if_no_trailing_newline
        (prev_file_newline, prev.name)
        (next_file_newline, next.name)
        ~warn_if_no_trailing_newline_in_both:config.warn_if_no_trailing_newline_in_both;
      compare_lines config ~prev:prev_lines ~next:next_lines ())
;;

let binary_different_message
      ~(config : Configuration.t)
      ~prev_file
      ~prev_is_binary
      ~next_file
      ~next_is_binary
  =
  match config.location_style with
  | Diff | None ->
    sprintf
      !"Files %{File_name#hum}%s and %{File_name#hum}%s differ"
      prev_file
      (if prev_is_binary then " (binary)" else "")
      next_file
      (if next_is_binary then " (binary)" else "")
  | Omake ->
    sprintf
      !"%s\n  File \"%{File_name#hum}\"\n  binary files differ\n"
      (Format.Location_style.omake_style_error_message_start
         ~file:(File_name.display_name prev_file)
         ~line:1)
      next_file
;;

(* Print hunks to stdout *)
let print hunks ~file_names ~(config : Configuration.t) =
  let prev_file, next_file = file_names in
  if Comparison_result.has_no_diff hunks
  then (
    if config.double_check
    then (
      match
        Unix.system
          (sprintf
             "cmp -s %s %s"
             (Sys.quote (File_name.real_name_exn prev_file))
             (Sys.quote (File_name.real_name_exn next_file)))
      with
      | Ok () -> ()
      | Error (`Exit_non_zero 1) ->
        printf "There are no differences except those filtered by your settings\n%!"
      | Error _ -> ()))
  else if (* Only print if -quiet is not set *)
    not config.quiet
  then (
    let output = config.output in
    let rules = config.rules in
    match hunks with
    | Binary_same -> assert false
    | Binary_different { prev_is_binary; next_is_binary } ->
      Printf.printf
        "%s\n"
        (binary_different_message
           ~config
           ~prev_file
           ~prev_is_binary
           ~next_file
           ~next_is_binary)
    | Hunks hunks ->
      Patdiff_core.print
        hunks
        ~file_names
        ~output
        ~rules
        ~location_style:config.location_style)
;;

let diff_files_internal (config : Configuration.t) ~prev_file ~next_file =
  let hunks = compare_files ~prev_file ~next_file config in
  print hunks ~file_names:(prev_file, next_file) ~config;
  if Comparison_result.has_no_diff hunks then `Same else `Different
;;

let with_alt (config : Configuration.t) ~prev ~next : File_name.t * File_name.t =
  ( Real { real_name = prev; alt_name = config.prev_alt }
  , Real { real_name = next; alt_name = config.next_alt } )
;;

let diff_files (config : Configuration.t) ~prev_file ~next_file =
  let prev_file, next_file = with_alt config ~prev:prev_file ~next:next_file in
  diff_files_internal config ~prev_file ~next_file
;;

(* Copied from string.ml, but preserves '\r's *)
let split_lines_preserve_slash_r =
  let back_up_at_newline ~t:_ ~pos ~eol =
    pos := !pos - 1;
    eol := !pos + 1;
  in
  fun t ->
    let n = String.length t in
    if n = 0
    then []
    else
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if Char.equal t.[!pos] '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if Char.( <> ) t.[!pos] '\n'
        then decr pos
        else
          (* Because [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := String.sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol
      done;
      String.sub t ~pos:0 ~len:!eol :: !ac
;;

let diff_strings
      ?print_global_header
      (config : Configuration.t)
      ~(prev : Diff_input.t)
      ~(next : Diff_input.t)
  =
  let lines { Diff_input.name = _; text } = split_lines_preserve_slash_r text |> Array.of_list in
  let hunks =
    Comparison_result.create
      config
      ~prev
      ~next
      ~compare_assuming_text:(fun config ~prev ~next ->
          let lines_prev = lines prev in
          let lines_next = lines next in
          compare_lines config ~prev_diff:prev ~next_diff:next ~prev:lines_prev ~next:lines_next ())
  in
  if Comparison_result.has_no_diff hunks
  then `Same
  else
    `Different
      (match hunks with
       | Binary_same -> assert false
       | Binary_different { prev_is_binary; next_is_binary } ->
         binary_different_message
           ~config
           ~prev_file:(Fake prev.name)
           ~prev_is_binary
           ~next_file:(Fake next.name)
           ~next_is_binary
       | Hunks hunks ->
         Patdiff_core.output_to_string
           hunks
           ?print_global_header
           ~file_names:(Fake prev.name, Fake next.name)
           ~output:config.output
           ~rules:config.rules
           ~location_style:config.location_style)
;;

let is_reg file =
  match Unix.stat (File_name.real_name_exn file) with
  | { st_kind = S_REG; _ } -> true
  | _ -> false
;;

let is_dir file =
  match Unix.stat (File_name.real_name_exn file) with
  | { st_kind = S_DIR; _ } -> true
  | _ -> false
;;

let rec diff_dirs_internal (config : Configuration.t) ~prev_dir ~next_dir ~file_filter =
  assert (is_dir prev_dir);
  assert (is_dir next_dir);
  let set_of_dir dir =
    (* Get a list of files for this directory only; do not descend farther
       (We recursively call diff_dirs later if we need to descend.) *)
    let file_filter =
      match file_filter with
      | None -> Fn.const true
      | Some file_filter -> file_filter
    in
    Sys.ls_dir (File_name.real_name_exn dir)
    |> List.filter ~f:(fun x ->
      let x = File_name.real_name_exn dir ^/ x in
      match Unix.stat x with
      | exception Unix.Unix_error (ENOENT, _, _) ->
        (* If the file disappeared during listing, let's pretend it didn't exist.
           This is important when the file is [-exclude]d because we don't want to create
           noise for excluded files, but it's also not too bad if the file is [-include]d
        *)
        false
      | stats -> file_filter (x, stats))
    |> String.Set.of_list
  in
  let prev_set = set_of_dir prev_dir in
  let next_set = set_of_dir next_dir in
  (* Get unique files *)
  let union = Set.union prev_set next_set in
  let prev_uniques = Set.diff union next_set in
  let next_uniques = Set.diff union prev_set in
  let handle_unique which file ~dir =
    printf !"Only in %{File_name#hum}: %s\n%!" dir file;
    (* Diff unique files against /dev/null, if desired *)
    if not config.mask_uniques
    then (
      let file = File_name.append dir file in
      if is_reg file
      then (
        let diff = diff_files_internal config in
        let null = File_name.dev_null in
        match which with
        | `Prev -> ignore (diff ~prev_file:file ~next_file:null : [ `Different | `Same ])
        | `Next -> ignore (diff ~prev_file:null ~next_file:file : [ `Different | `Same ])))
  in
  Set.iter prev_uniques ~f:(handle_unique `Prev ~dir:prev_dir);
  Set.iter next_uniques ~f:(handle_unique `Next ~dir:next_dir);
  (* Get differences *)
  let inter = Set.inter prev_set next_set in
  let exit_code = ref `Same in
  let diff file =
    let prev_file = File_name.append prev_dir file in
    let next_file = File_name.append next_dir file in
    if is_reg prev_file && is_reg next_file
    then (
      let hunks = compare_files ~prev_file ~next_file config in
      if not (Comparison_result.has_no_diff hunks)
      then (
        exit_code := `Different;
        (* Print the diff if not -quiet *)
        match config.quiet with
        | false -> print hunks ~file_names:(prev_file, next_file) ~config
        | true ->
          printf
            !"Files %{File_name#hum} and %{File_name#hum} differ\n%!"
            prev_file
            next_file))
    else if is_dir prev_file && is_dir next_file
    then
      if not config.shallow
      then (
        match
          diff_dirs_internal ~prev_dir:prev_file ~next_dir:next_file config ~file_filter
        with
        | `Same -> ()
        | `Different -> exit_code := `Different)
      else
        printf
          !"Common subdirectories: %{File_name#hum} and %{File_name#hum}\n%!"
          prev_file
          next_file
    else (
      exit_code := `Different;
      printf
        !"Files %{File_name#hum} and %{File_name#hum} are not the same type\n%!"
        prev_file
        next_file)
  in
  Set.iter inter ~f:diff;
  if Set.is_empty prev_uniques && Set.is_empty next_uniques
  then !exit_code
  else `Different
;;

let diff_dirs (config : Configuration.t) ~prev_dir ~next_dir ~file_filter =
  let prev_dir, next_dir = with_alt config ~prev:prev_dir ~next:next_dir in
  if not (is_dir prev_dir)
  then
    invalid_argf !"diff_dirs: prev_dir '%{File_name#hum}' is not a directory" prev_dir ();
  if not (is_dir next_dir)
  then
    invalid_argf !"diff_dirs: next_dir '%{File_name#hum}' is not a directory" next_dir ();
  diff_dirs_internal config ~prev_dir ~next_dir ~file_filter
;;
