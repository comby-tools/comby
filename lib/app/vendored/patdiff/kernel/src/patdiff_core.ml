open! Core_kernel
open! Import
include Patdiff_core_intf

include struct
  open Configuration

  let default_context = default_context
  let default_line_big_enough = default_line_big_enough
  let default_word_big_enough = default_word_big_enough
end

(* Strip whitespace from a string by stripping and replacing with spaces *)
let ws_rex = Re.compile Re.(rep1 space)
let ws_rex_anchored = Re.compile Re.(seq [ bol; rep space; eol ])
let ws_sub = " "
let remove_ws s = String.strip (Re.replace_string ws_rex s ~by:ws_sub)
let is_ws = Re.execp ws_rex_anchored

(* This regular expression describes the delimiters on which to split the string *)
let words_rex =
  let open Re in
  let delim = set {|"{}[]#,.;()_|} in
  let punct = rep1 (set {|=`+-/!@$%^&*:|}) in
  let space = rep1 space in
  (* We don't want to split up ANSI color sequences, so let's make sure they get through
     intact. *)
  let ansi_sgr_sequence =
    let esc = char '\027' in
    seq [ esc; char '['; rep (alt [ char ';'; digit ]); char 'm' ]
  in
  compile (alt [ delim; punct; space; ansi_sgr_sequence ])
;;

(* Split a string into a list of string options delimited by words_rex
   (delimiters included) *)
let split s ~keep_ws =
  let s = if keep_ws then s else String.rstrip s in
  if String.is_empty s && keep_ws
  then [ "" ]
  else
    Re.split_full words_rex s
    |> List.filter_map ~f:(fun token ->
      let string =
        match token with
        | `Delim d -> Re.Group.get d 0
        | `Text t -> t
      in
      if String.is_empty string then None else Some string)
;;

(* This function ensures that the tokens passed to Patience diff do not include
   whitespace.  Whitespace is appended to words, and then removed by [~transform] later
   on. The point is to make the semantic cleanup go well -- we don't want whitespace
   matches to "count" as part of the length of a match. *)
let whitespace_ignorant_split s =
  if String.is_empty s
  then []
  else (
    let istext s = not (Re.execp ws_rex s) in
    split s ~keep_ws:false
    |> List.group ~break:(fun split_result1 _ -> istext split_result1)
    |> List.map ~f:String.concat)
;;

include struct
  let%expect_test _ =
    print_s ([%sexp_of: string list] (split ~keep_ws:true ""));
    [%expect {| ("") |}]
  ;;
end

module Make (Output_impls : Output_impls) = struct
  module Output_ops = struct
    module Rule = struct
      let apply text ~rule ~output ~refined =
        let (module O) = Output_impls.implementation output in
        O.Rule.apply text ~rule ~refined
      ;;
    end

    module Rules = struct
      let to_string (rules : Format.Rules.t) output
        : string Patience_diff.Range.t -> string Patience_diff.Range.t
        =
        let apply text ~rule ~refined = Rule.apply text ~rule ~output ~refined in
        function
        | Same ar ->
          let formatted_ar =
            Array.map ar ~f:(fun (x, y) ->
              let app = apply ~rule:rules.line_same ~refined:false in
              app x, app y)
          in
          Same formatted_ar
        | Next ar -> Next (Array.map ar ~f:(apply ~refined:false ~rule:rules.line_next))
        | Prev ar -> Prev (Array.map ar ~f:(apply ~refined:false ~rule:rules.line_prev))
        | Unified ar ->
          Unified (Array.map ar ~f:(apply ~refined:true ~rule:rules.line_unified))
        | Replace (ar1, ar2) ->
          let ar1 = Array.map ar1 ~f:(apply ~refined:true ~rule:rules.line_prev) in
          let ar2 = Array.map ar2 ~f:(apply ~refined:true ~rule:rules.line_next) in
          Replace (ar1, ar2)
      ;;

      let map_ranges (hunks : _ Patience_diff.Hunk.t list) ~f =
        List.map hunks ~f:(fun hunk -> { hunk with ranges = List.map hunk.ranges ~f })
      ;;

      let apply hunks ~rules ~output = map_ranges hunks ~f:(to_string rules output)
    end

    let print
          ~print_global_header
          ~file_names
          ~rules
          ~output
          ~print
          ~location_style
          hunks
      =
      let formatted_hunks = Rules.apply ~rules ~output hunks in
      let (module O) = Output_impls.implementation output in
      O.print
        ~print_global_header
        ~file_names
        ~rules
        ~print
        ~location_style
        formatted_hunks
    ;;
  end

  let diff ~context ~line_big_enough ~keep_ws ~prev ~next =
    let transform = if keep_ws then Fn.id else remove_ws in
    Patience_diff.String.get_hunks
      ~transform
      ~context
      ~big_enough:line_big_enough
      ~prev
      ~next
  ;;

  type word_or_newline =
    [ `Newline of int * string option (* (number of newlines, subsequent_whitespace) *)
    | `Word of string
    ]
  [@@deriving sexp_of]

  (* Splits an array of lines into an array of pieces (`Newlines and R.Words) *)
  let explode ar ~keep_ws =
    let words = Array.to_list ar in
    let words =
      if keep_ws
      then List.map words ~f:(split ~keep_ws)
      else List.map words ~f:whitespace_ignorant_split
    in
    let to_words l = List.map l ~f:(fun s -> `Word s) in
    (*
       [`Newline of (int * string option)]

       can be thought of as:

       [`Newline of
       ([`How_many_consecutive_newlines of int]
     * [`Some_subsequent_whitespace of string
       |`Empty_string
       ])]

       This representation is used to try to collapse consecutive whitespace as tightly as
       possible, but it's not a great abstraction, so some consecutive whitespace does not
       get collapsed.

    *)
    let words =
      List.concat_map words ~f:(fun x ->
        match x with
        | hd :: tl ->
          if keep_ws && (not (String.is_empty hd)) && is_ws hd
          then `Newline (1, Some hd) :: to_words tl
          else `Newline (1, None) :: `Word hd :: to_words tl
        | [] -> [ `Newline (1, None) ])
    in
    let words =
      List.fold_right words ~init:[] ~f:(fun x acc ->
        (* look back at what we've accumulated so far to see if there's any whitespace that
           can be collapsed. *)
        match acc with
        | `Word s :: tl -> x :: `Word s :: tl
        | `Newline (i, None) :: tl ->
          (match x with
           | `Word s -> `Word s :: `Newline (i, None) :: tl
           | `Newline (j, opt) ->
             (* collapse the whitespace from each [`Newline] by summing
                how_many_consecutive_newlines from each (i+j) *)
             `Newline (i + j, opt) :: tl)
        | `Newline (i, Some s1) :: tl ->
          (match x with
           | `Word s2 -> `Word s2 :: `Newline (i, Some s1) :: tl
           | `Newline (j, opt) ->
             (* collapse the whitespace from each [`Newline] by concatenating any
                subsequent_whitespace (opt ^ s1) and summing how_many_consecutive_newlines
                (i+j) from each. *)
             let s1 = Option.value opt ~default:"" ^ s1 in
             `Newline (i + j, Some s1) :: tl)
        | [] -> [ x ])
    in
    (* Throw away the very first `Newline *)
    let words =
      match words with
      | `Newline (i, opt) :: tl -> `Newline (i - 1, opt) :: tl
      | `Word _ :: _ | [] ->
        raise_s
          [%message
            "Expected words to start with a `Newline." (words : word_or_newline list)]
    in
    (* Append a newline to the end, if this array has any words *)
    let words =
      match words with
      | [] -> []
      | [ `Newline (0, None) ] -> []
      | list -> List.append list [ `Newline (1, None) ]
    in
    Array.of_list words
  ;;

  (* Takes hunks of `Words and `Newlines and collapses them back into lines,
   * formatting appropriately. *)
  let collapse ranges ~rule_same ~rule_prev ~rule_next ~kind ~output =
    (* flag indicates what kind of range is currently being collapsed *)
    let flag = ref `Same in
    (* segment is the current series of words being processed. *)
    let segment = ref [] in
    (* line is the current series of formatted segments *)
    let line = ref [] in
    (* lines is the return array *)
    let lines = ref [] in
    let apply ~rule = function
      | "" -> ""
      | s -> Output_ops.Rule.apply s ~rule ~output ~refined:false
    in
    (*
     * Finish the current segment by applying the appropriate format
     * and popping it on to the end of the current line
    *)
    let finish_segment () =
      let rule =
        match !flag with
        | `Same -> rule_same
        | `Prev -> rule_prev
        | `Next -> rule_next
      in
      let formatted_segment = List.rev !segment |> String.concat |> apply ~rule in
      line := formatted_segment :: !line;
      segment := []
    in
    (*
     * Finish the current segment, apply the reset rule to the line,
     * and pop the finished line onto the return array
    *)
    let newline i =
      for _ = 1 to i do
        finish_segment ();
        lines := String.concat (List.rev !line) :: !lines;
        line := []
      done
    in
    let f range =
      (* Extract the array, set flag appropriately, *)
      let ar =
        match (range : _ Patience_diff.Range.t) with
        | Same ar ->
          flag := `Same;
          (* R.Same ar is an array of tuples.  The first tuple is an
           * element from the old file, the second tuple, an element
           * from the new file.  Depending on what kind of collapse
           * this is, we want only one or the other. *)
          let f =
            match kind with
            | `Prev_only -> fst
            | `Next_only -> snd
            | `Unified -> snd
          in
          Array.map ar ~f
        | Prev ar ->
          flag := `Prev;
          ar
        | Next ar ->
          flag := `Next;
          ar
        | Replace _ | Unified _ ->
          (* When calling collapse, we always call
           * Patience_diff.unified first, which removes all R.Replaces
           * and R.Unifieds. *)
          assert false
      in
      (* Iterate through the elements of the range, appending each `Word to
       * segment and calling newline on each `Newline
      *)
      Array.iter ar ~f:(function
        | `Newline (i, None) -> newline i
        | `Newline (i, Some s) ->
          newline i;
          segment := s :: !segment
        | `Word s -> segment := s :: !segment);
      finish_segment ()
    in
    List.iter ranges ~f;
    (match !line with
     | [] | [ "" ] -> ()
     | line ->
       let line = String.concat (List.rev line) in
       if is_ws line
       then
         (* This branch was unreachable in our regression tests, but I can't prove it's
            unreachable in all cases. Rather than raise in production, let's drop this
            whitespace. *)
         ()
       else
         raise_s
           [%message
             "Invariant violated: [collapse] got a line not terminated with a newline"
               (line : string)]);
    Array.of_list (List.rev !lines)
  ;;

  (* Get the hunks from two arrays of pieces (`Words and `Newlines) *)
  let diff_pieces ~prev_pieces ~next_pieces ~keep_ws ~word_big_enough =
    let context = -1 in
    let transform =
      if keep_ws
      then
        function
        | `Word s -> s
        | `Newline (lines, trailing_whitespace) ->
          Option.fold trailing_whitespace ~init:(String.make lines '\n') ~f:String.( ^ )
      else
        function
        | `Word s -> remove_ws s
        | `Newline (0, _) -> ""
        | `Newline (_, _) -> " "
    in
    Patience_diff.String.get_hunks
      ~transform
      ~context
      ~big_enough:word_big_enough
      ~prev:prev_pieces
      ~next:next_pieces
  ;;

  let ranges_are_just_whitespace (ranges : _ Patience_diff.Range.t list) =
    List.for_all ranges ~f:(function
      | Prev piece_array | Next piece_array ->
        Array.for_all piece_array ~f:(function
          | `Word s -> String.is_empty (remove_ws s)
          | `Newline _ -> true)
      | _ -> true)
  ;;

  (* Interleaves the display of minus lines and plus lines so that equal words are presented
     close together.  There is some heuristic for when we think doing this improves the
     diff. *)
  let split_for_readability rangelist =
    let ans : _ Patience_diff.Range.t list list ref = ref [] in
    let pending_ranges : _ Patience_diff.Range.t list ref = ref [] in
    let append_range range = pending_ranges := range :: !pending_ranges in
    List.iter rangelist ~f:(fun range ->
      let split_was_executed =
        match (range : _ Patience_diff.Range.t) with
        | Next _ | Prev _ | Replace _ | Unified _ -> false
        | Same seq ->
          let first_newline =
            Array.find_mapi seq ~f:(fun i ->
              function
              | `Word _, _ | _, `Word _ | `Newline (0, _), _ | _, `Newline (0, _) ->
                None
              | `Newline first_nlA, `Newline first_nlB -> Some (i, first_nlA, first_nlB))
          in
          (match first_newline with
           | None -> false
           | Some (i, first_nlA, first_nlB) ->
             if Array.length seq - i <= Configuration.too_short_to_split
             then false
             else (
               append_range (Same (Array.sub seq ~pos:0 ~len:i));
               (* A non-zero `Newline is required for [collapse] to work properly. *)
               append_range (Same [| `Newline (1, None), `Newline (1, None) |]);
               ans := List.rev !pending_ranges :: !ans;
               pending_ranges := [];
               let suf = Array.sub seq ~pos:i ~len:(Array.length seq - i) in
               let decr_first (x, y) = x - 1, y in
               suf.(0)
               <- (`Newline (decr_first first_nlA), `Newline (decr_first first_nlB));
               append_range (Same suf);
               true))
      in
      if not split_was_executed then append_range range);
    List.rev
      (match !pending_ranges with
       | [] -> !ans
       | _ :: _ as ranges -> List.rev ranges :: !ans)
  ;;

  (* Refines the diff, splitting the lines into smaller arrays and diffing them, then
     collapsing them back into their initial lines after applying a format. *)
  let refine
        ~(rules : Format.Rules.t)
        ~produce_unified_lines
        ~output
        ~keep_ws
        ~split_long_lines
        ~interleave
        ~word_big_enough
        (hunks : string Patience_diff.Hunk.t list)
    =
    let rule_prev = rules.word_prev in
    let rule_next = rules.word_next in
    let collapse = collapse ~rule_prev ~rule_next ~output in
    let () =
      match output with
      | Ansi | Html -> ()
      | Ascii ->
        if produce_unified_lines
        then failwith "produce_unified_lines is not supported in Ascii mode"
    in
    let console_width =
      lazy
        (match Output_impls.console_width () with
         | Error _ -> 80
         | Ok width -> width)
    in
    let refine_range : _ Patience_diff.Range.t -> _ Patience_diff.Range.t list = function
      | Next a when (not keep_ws) && Array.for_all a ~f:is_ws ->
        [ Same (Array.zip_exn a a) ]
      | Prev a when (not keep_ws) && Array.for_all a ~f:is_ws -> []
      | (Next _ | Prev _ | Same _ | Unified _) as range -> [ range ]
      | Replace (prev_ar, next_ar) ->
        (* Explode the arrays *)
        let prev_pieces = explode prev_ar ~keep_ws in
        let next_pieces = explode next_ar ~keep_ws in
        (* Diff the pieces *)
        let sub_diff = diff_pieces ~prev_pieces ~next_pieces ~keep_ws ~word_big_enough in
        (* Smash the hunks' ranges all together *)
        let sub_diff = Patience_diff.Hunks.ranges sub_diff in
        (* Break it up where lines are too long *)
        let sub_diff_pieces =
          if not split_long_lines
          then [ sub_diff ]
          else (
            let max_len = Int.max 20 (force console_width - 2) in
            (* Accumulates the total length of the line so far, summing lengths
               of word tokens but resetting when newlines are hit *)
            let get_new_len_so_far ~len_so_far tokens_arr =
              Array.fold ~init:len_so_far tokens_arr ~f:(fun len_so_far token ->
                match token with
                | `Newline _ -> 0
                | `Word word -> len_so_far + String.length word)
            in
            (* Iteratively split long lines up.
               Produces a list of "range lists", where each range list should be displayed
               all together in one unbroken piece before being followed by the next range
               list, etc. *)
            let rec split_lines len_so_far sub_diff rangeaccum rangelistaccum =
              match sub_diff with
              | [] ->
                (match rangeaccum with
                 | [] -> List.rev rangelistaccum
                 | _ -> List.rev (List.rev rangeaccum :: rangelistaccum))
              (* More tokens ranges left to process *)
              | range :: rest ->
                (match (range : _ Patience_diff.Range.t) with
                 | Same tokenpairs_arr ->
                   let range_of_tokens tokenpairs =
                     Patience_diff.Range.Same (Array.of_list tokenpairs)
                   in
                   (* Keep taking tokens until we exceed max_len or hit a newline.
                      Returns (new len_so_far, new range, remaining tokens)*)
                   let rec take_until_max len_so_far tokenpairs accum =
                     match tokenpairs with
                     | [] -> len_so_far, range_of_tokens (List.rev accum), []
                     | ((token, _) as tokenpair) :: rest ->
                       (match token with
                        | `Newline _ ->
                          0, range_of_tokens (List.rev (tokenpair :: accum)), rest
                        | `Word word ->
                          let wordlen = String.length word in
                          if wordlen + len_so_far > max_len && len_so_far > 0
                          then 0, range_of_tokens (List.rev accum), tokenpairs
                          else
                            take_until_max (wordlen + len_so_far) rest (tokenpair :: accum))
                   in
                   let make_newline () =
                     Patience_diff.Range.Same [| `Newline (1, None), `Newline (1, None) |]
                   in
                   (* Keep taking ranges until all tokens exhausted.
                      Returns (new len_so_far, range list) *)
                   let rec take_ranges_until_exhausted len_so_far tokenpairs accum =
                     match tokenpairs with
                     | [] -> len_so_far, List.rev accum
                     | _ ->
                       let new_len_so_far, new_range, new_tokenpairs =
                         take_until_max len_so_far tokenpairs []
                       in
                       let new_accum = `Range new_range :: accum in
                       (* If there are token pairs left, that means we hit the max_len,
                          so add a break at this point *)
                       let new_accum =
                         match new_tokenpairs with
                         | _ :: _ -> `Break :: `Range (make_newline ()) :: new_accum
                         | [] -> new_accum
                       in
                       take_ranges_until_exhausted new_len_so_far new_tokenpairs new_accum
                   in
                   let new_len_so_far, new_ranges =
                     take_ranges_until_exhausted
                       len_so_far
                       (Array.to_list tokenpairs_arr)
                       []
                   in
                   (* Update rangeaccum and rangelistaccum according to the `Ranges and
                      `Breaks. `Ranges accumulate on to the existing range list to be
                      displayed contiguously, `Breaks start a new range list. *)
                   let rangeaccum, rangelistaccum =
                     List.fold
                       new_ranges
                       ~init:(rangeaccum, rangelistaccum)
                       ~f:(fun (rangeaccum, rangelistaccum) r ->
                         match r with
                         | `Break -> [], List.rev rangeaccum :: rangelistaccum
                         | `Range r -> r :: rangeaccum, rangelistaccum)
                   in
                   split_lines new_len_so_far rest rangeaccum rangelistaccum
                 | Next tokens_arr | Prev tokens_arr ->
                   let new_len_so_far = get_new_len_so_far ~len_so_far tokens_arr in
                   split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                 | Replace (prev_arr, next_arr) ->
                   let new_len_so_far =
                     Int.max
                       (get_new_len_so_far ~len_so_far prev_arr)
                       (get_new_len_so_far ~len_so_far next_arr)
                   in
                   split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                 | Unified _ -> assert false)
            in
            split_lines 0 sub_diff [] [])
        in
        let sub_diff_pieces =
          if interleave
          then List.concat_map sub_diff_pieces ~f:split_for_readability
          else sub_diff_pieces
        in
        List.concat_map sub_diff_pieces ~f:(fun sub_diff ->
          let sub_prev = Patience_diff.Range.prev_only sub_diff in
          let sub_next = Patience_diff.Range.next_only sub_diff in
          let all_same ranges =
            List.for_all ranges ~f:(fun range ->
              match (range : _ Patience_diff.Range.t) with
              | Same _ -> true
              | Prev a | Next a ->
                if keep_ws
                then false
                else
                  Array.for_all a ~f:(function
                    | `Newline _ -> true
                    | `Word _ -> false)
              | _ -> false)
          in
          let prev_all_same = all_same sub_prev in
          let next_all_same = all_same sub_next in
          let produce_unified_lines =
            produce_unified_lines
            && (((not (ranges_are_just_whitespace sub_prev)) && next_all_same)
                || ((not (ranges_are_just_whitespace sub_next)) && prev_all_same))
          in
          (* Collapse the pieces back into lines *)
          let prev_next_pairs =
            match prev_all_same, next_all_same with
            | true, true ->
              let kind = `Next_only in
              let rule_same = rules.word_same_unified in
              let next_ar = collapse sub_next ~rule_same ~kind in
              [ next_ar, next_ar ]
            | false, true ->
              let kind = `Prev_only in
              let rule_same =
                if produce_unified_lines
                then rules.word_same_unified
                else rules.word_same_prev
              in
              let prev_ar = collapse sub_prev ~rule_same ~kind in
              let kind = `Next_only in
              let rule_same = rules.word_same_next in
              let next_ar = collapse sub_next ~rule_same ~kind in
              [ prev_ar, next_ar ]
            | true, false ->
              let kind = `Next_only in
              let rule_same =
                if produce_unified_lines
                then rules.word_same_unified
                else rules.word_same_next
              in
              let next_ar = collapse sub_next ~rule_same ~kind in
              let kind = `Prev_only in
              let rule_same = rules.word_same_prev in
              let prev_ar = collapse sub_prev ~rule_same ~kind in
              [ prev_ar, next_ar ]
            | false, false ->
              let kind = `Prev_only in
              let rule_same = rules.word_same_prev in
              let prev_ar = collapse sub_prev ~rule_same ~kind in
              let kind = `Next_only in
              let rule_same = rules.word_same_next in
              let next_ar = collapse sub_next ~rule_same ~kind in
              [ prev_ar, next_ar ]
          in
          List.map prev_next_pairs ~f:(fun (prev_ar, next_ar) ->
            let range : _ Patience_diff.Range.t =
              match prev_all_same, next_all_same with
              | true, true -> Same (Array.map next_ar ~f:(fun x -> x, x))
              | _ ->
                (match prev_ar, next_ar with
                 (* Ugly hack that takes care of empty files *)
                 | [| "" |], next_ar -> Replace ([||], next_ar)
                 | prev_ar, [| "" |] -> Replace (prev_ar, [||])
                 | prev_ar, next_ar ->
                   (match produce_unified_lines, prev_all_same, next_all_same with
                    | true, true, false -> Unified next_ar
                    | true, false, true -> Unified prev_ar
                    | false, _, _ | _, false, false -> Replace (prev_ar, next_ar)
                    | _ -> assert false))
            in
            range))
    in
    hunks
    |> List.map ~f:(fun hunk ->
      { hunk with ranges = List.concat_map hunk.ranges ~f:refine_range })
    |> List.filter ~f:(not << Patience_diff.Hunk.all_same)
  ;;

  let print ~file_names ~rules ~output ~location_style hunks =
    Output_ops.print
      hunks
      ~rules
      ~output
      ~file_names
      ~print:(Printf.printf "%s\n")
      ~location_style
      ~print_global_header:true
  ;;

  let output_to_string
        ?(print_global_header = false)
        ~file_names
        ~rules
        ~output
        ~location_style
        hunks
    =
    let buf = Queue.create () in
    Output_ops.print
      hunks
      ~file_names
      ~location_style
      ~output
      ~print_global_header
      ~print:(Queue.enqueue buf)
      ~rules;
    String.concat (Queue.to_list buf) ~sep:"\n"
  ;;

  let iter_ansi ~rules ~f_hunk_break ~f_line hunks =
    let hunks = Output_ops.Rules.apply hunks ~rules ~output:Ansi in
    Hunks.iter ~f_hunk_break ~f_line hunks
  ;;

  let patdiff
        ?(context = Configuration.default_context)
        ?(keep_ws = false)
        ?(rules = Format.Rules.default)
        ?(output = Output.Ansi)
        ?(produce_unified_lines = true)
        ?(split_long_lines = true)
        ?print_global_header
        ?(location_style = Format.Location_style.Diff)
        ?(interleave = true)
        ?(line_big_enough = Configuration.default_line_big_enough)
        ?(word_big_enough = Configuration.default_word_big_enough)
        ~(prev : Diff_input.t)
        ~(next : Diff_input.t)
        ()
    =
    let keep_ws = keep_ws || Should_keep_whitespace.for_diff ~prev ~next in
    let hunks =
      diff
        ~context
        ~keep_ws
        ~line_big_enough
        ~prev:(List.to_array (String.split_lines prev.text))
        ~next:(List.to_array (String.split_lines next.text))
      |> refine
           ~rules
           ~produce_unified_lines
           ~output
           ~keep_ws
           ~split_long_lines
           ~interleave
           ~word_big_enough
    in
    output_to_string
      ?print_global_header
      ~file_names:(Fake prev.name, Fake next.name)
      ~rules
      ~output
      ~location_style
      hunks
  ;;
end

module Without_unix = Make (struct
    let console_width () = Ok 80

    let implementation : Output.t -> (module Output.S) = function
      | Ansi -> (module Ansi_output)
      | Ascii -> (module Ascii_output)
      | Html -> (module Html_output.Without_mtime)
    ;;
  end)

module Private = struct
  module Make = Make
end
