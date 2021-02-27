open! Core_kernel
open! Import
module Range = Patience_diff.Range
module Hunk = Patience_diff.Hunk
module Hunks = Patience_diff.Hunks

module String_with_floats = struct
  type t =
    { floats : float array
    ; without_floats : string
    }
  [@@deriving sexp]

  let close_enough tolerance =
    let equal f f' =
      Float.( <= )
        (Float.abs (f -. f'))
        (Percent.apply tolerance (Float.min (Float.abs f) (Float.abs f')))
    in
    stage (fun t t' ->
      String.( = ) t.without_floats t'.without_floats
      && Array.equal equal t.floats t'.floats)
  ;;

  let float_regex =
    lazy
      (let open Re in
       let delim = set {| ;:,\|#&(){}[]<>~=+-*/|} in
       let prefix = group (alt [ start; char '$'; delim ]) in
       let float =
         group
           (seq
              [ opt (char '-')
              ; rep1 digit
              ; opt (seq [ char '.'; opt (rep1 digit) ])
              ; opt (seq [ set {|eE|}; opt (set {|+-|}); rep1 digit ])
              ])
       in
       let suffix =
         let suffix_with_delim = alt [ stop; char '%'; delim ] in
         let suffix_with_unit =
           let unit = alt [ str "bp"; str "s"; str "m"; str "ms" ] in
           seq [ unit; eow ]
         in
         group (alt [ suffix_with_delim; suffix_with_unit ])
       in
       compile (seq [ prefix; float; suffix ]))
  ;;

  let create s =
    let rec loop floats line =
      match Re.exec_opt (force float_regex) line with
      | None -> { floats = Array.of_list_rev floats; without_floats = line }
      | Some groups ->
        let float = Float.of_string (Re.Group.get groups 2) in
        let line =
          Re.replace (force float_regex) line ~all:false ~f:(fun groups ->
            let prefix = Re.Group.get groups 1 in
            let suffix = Re.Group.get groups 3 in
            prefix ^ suffix)
        in
        loop (float :: floats) line
    in
    loop [] s
  ;;

  include struct
    let%expect_test "trailing [.]" =
      let t = create "(foo 12.)" in
      print_s [%message (t : t)];
      [%expect {| (t ((floats (12)) (without_floats "(foo )"))) |}]
    ;;

    let%expect_test "scientific notation" =
      let t1 = create "(foo -12345678910.11)" in
      let t2 = create "(foo -1.234567891011e10)" in
      let t3 = create "(foo -1.234567891011e+10)" in
      let t4 = create "(foo -1.234567891011E10)" in
      let t5 = create "(foo -1.234567891011E+10)" in
      let t6 = create "(foo -123456789101.1e-1)" in
      let t7 = create "(foo -1234567891011.e-2)" in
      let t8 = create "(foo -1234567891011e-2)" in
      print_s
        [%message
          (t1 : t) (t2 : t) (t3 : t) (t4 : t) (t5 : t) (t6 : t) (t7 : t) (t8 : t)];
      [%expect
        {|
        ((t1 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t2 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t3 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t4 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t5 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t6 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t7 ((floats (-12345678910.11)) (without_floats "(foo )")))
         (t8 ((floats (-12345678910.11)) (without_floats "(foo )")))) |}]
    ;;

    let%expect_test _ =
      let prev = create "(dynamic (Ok ((price_range (-18.8305 39.1095)))))\n" in
      let next = create "(dynamic (Ok ((price_range (-18.772 38.988)))))\n" in
      print_s [%message (prev : t) (next : t)];
      [%expect
        {|
        ((prev
          ((floats (-18.8305 39.1095))
           (without_floats "(dynamic (Ok ((price_range ( )))))\n")))
         (next
          ((floats (-18.772 38.988))
           (without_floats "(dynamic (Ok ((price_range ( )))))\n")))) |}]
    ;;

    let%expect_test _ =
      let prev =
        create "(primary_exchange_core_session (09:30:00.000000 16:00:00.000000))"
      in
      let next =
        create "(primary_exchange_core_session (09:30:00.000000 15:59:00.000000))"
      in
      print_s [%message (prev : t) (next : t)];
      [%expect
        {|
        ((prev
          ((floats (9 30 0 16 0 0))
           (without_floats "(primary_exchange_core_session (:: ::))")))
         (next
          ((floats (9 30 0 15 59 0))
           (without_floats "(primary_exchange_core_session (:: ::))")))) |}]
    ;;
  end
end

(* If [a = needleman_wunsch xs ys], [a.(i).(j)] is the Levenshtein distance between the
   first [i] elts of [xs] and the first [j] elts of [ys]. This corresponds to
   Needleman-Wunsch where matches are 0, and mismatch, insert, and delete are all 1. *)
let needleman_wunsch xs ys ~equal =
  let min3 a b c = Int.min (Int.min a b) c in
  let rows = Array.length xs in
  let cols = Array.length ys in
  let a =
    let rows = rows + 1 in
    let cols = cols + 1 in
    Array.init rows ~f:(fun _ -> Array.create ~len:cols Int.max_value)
  in
  for i = 0 to rows do
    a.(i).(0) <- i
  done;
  for j = 0 to cols do
    a.(0).(j) <- j
  done;
  for i = 1 to rows do
    for j = 1 to cols do
      a.(i).(j)
      <- min3
           (a.(i - 1).(j) + 1)
           (a.(i).(j - 1) + 1)
           (a.(i - 1).(j - 1) + if equal xs.(i - 1) ys.(j - 1) then 0 else 1)
    done
  done;
  a
;;

type partial_range_indexes =
  | Matching of (int * int) list
  | Nonmatching of int list * int list

(* [recover_ranges a xs ys] does the traceback step of Needleman-Wunsch to find the
   lowest-scoring [Range.t list] that transforms [xs] into [ys]. *)
let recover_ranges xs ys a =
  (* index of smallest element in triple, with ties going to the element with higher
     index *)
  let smallest a b c =
    if a < b then if a < c then 0 else 2 else if b < c then 1 else 2
  in
  let cons_minus_one car cdr ~if_unequal_to =
    if car = if_unequal_to then cdr else (car - 1) :: cdr
  in
  let rec traceback a i j acc =
    if i <= 0 || j <= 0
    then
      if i <= 0 && j <= 0
      then acc
      else (
        let is' = List.range 0 i in
        let js' = List.range 0 j in
        match acc with
        | [] | Matching _ :: _ -> Nonmatching (is', js') :: acc
        | Nonmatching (is, js) :: acc -> Nonmatching (is' @ is, js' @ js) :: acc)
    else (
      let i', j', matched =
        match smallest a.(i - 1).(j) a.(i - 1).(j - 1) a.(i).(j - 1) with
        | 0 -> i - 1, j, false
        | 1 -> i - 1, j - 1, a.(i).(j) = a.(i - 1).(j - 1)
        | 2 -> i, j - 1, false
        | _ -> failwith "smallest only returns 0, 1, or 2."
      in
      let acc =
        if matched
        then (
          match acc with
          | [] | Nonmatching _ :: _ -> Matching [ i - 1, j - 1 ] :: acc
          | Matching ijs :: acc -> Matching ((i - 1, j - 1) :: ijs) :: acc)
        else (
          match acc with
          | [] | Matching _ :: _ ->
            Nonmatching
              ( cons_minus_one i [] ~if_unequal_to:i'
              , cons_minus_one j [] ~if_unequal_to:j' )
            :: acc
          | Nonmatching (is, js) :: acc ->
            Nonmatching
              ( cons_minus_one i is ~if_unequal_to:i'
              , cons_minus_one j js ~if_unequal_to:j' )
            :: acc)
      in
      traceback a i' j' acc)
  in
  let elts_of_indices is xs = Array.of_list is |> Array.map ~f:(Array.get xs) in
  traceback a (Array.length xs) (Array.length ys) []
  |> List.map ~f:(function
    | Matching ijs ->
      let xys = Array.of_list ijs |> Array.map ~f:(fun (i, j) -> xs.(i), ys.(j)) in
      Range.Same xys
    | Nonmatching (is, []) -> Prev (elts_of_indices is xs)
    | Nonmatching ([], js) -> Next (elts_of_indices js ys)
    | Nonmatching (is, js) -> Replace (elts_of_indices is xs, elts_of_indices js ys))
;;

let%expect_test "recover_ranges" =
  let prev = [| "a"; "b" |] in
  let next = [| "z" |] in
  let a = needleman_wunsch prev next ~equal:String.equal in
  print_s ([%sexp_of: int array array] a);
  [%expect {| ((0 1) (1 1) (2 2)) |}];
  let ranges = recover_ranges prev next a in
  print_s ([%sexp_of: string Range.t list] ranges);
  [%expect {| ((Replace (a b) (z))) |}]
;;

let do_tolerance ~equal hunks =
  Hunks.concat_map_ranges hunks ~f:(fun range ->
    match (range : string Range.t) with
    | Same _ | Prev _ | Next _ -> [ range ]
    | Unified _ ->
      raise_s [%message "Unexpected Unified range." ~_:(range : string Range.t)]
    | Replace (prev, next) ->
      needleman_wunsch
        (Array.map prev ~f:String_with_floats.create)
        (Array.map next ~f:String_with_floats.create)
        ~equal
      |> recover_ranges prev next)
;;

module Context_limit : sig
  val enforce : context:int -> string Hunk.t -> string Hunk.t list
end = struct
  module Merged_with_position : sig
    module Position : sig
      type t =
        | Start
        | Middle
        | End
      [@@deriving sexp_of]
    end

    type t = string Range.t * Position.t [@@deriving sexp_of]

    val f : string Range.t list -> t Sequence.t
  end = struct
    module Position = struct
      type t =
        | Start
        | Middle
        | End
      [@@deriving sexp_of]
    end

    open Position

    type t = string Range.t * Position.t [@@deriving sexp_of]

    let f = function
      | [] -> Sequence.empty
      | car :: cdr ->
        Sequence.unfold_with_and_finish
          (Sequence.of_list cdr : string Range.t Sequence.t)
          ~init:(car, Start)
          ~running_step:(fun (car, pos) cadr ->
            match car, cadr with
            | Same car_lines, Same cadr_lines ->
              Skip (Same (Array.concat [ car_lines; cadr_lines ]), pos)
            | Unified _, _ | _, Unified _ ->
              raise_s
                [%message
                  "Unexpected unified range."
                    (car : string Range.t)
                    (cadr : string Range.t)]
            | (Prev _ | Next _ | Replace _), (Prev _ | Next _ | Replace _)
            | Same _, (Prev _ | Next _ | Replace _)
            | (Prev _ | Next _ | Replace _), Same _ -> Yield ((car, pos), (cadr, Middle)))
          ~inner_finished:(fun (last, pos) ->
            match last, pos with
            | Unified _, _ ->
              raise_s [%message "Unexpected unified range." ~_:(last : string Range.t)]
            | _, End ->
              raise_s [%message "Produced End in running step." (last : string Range.t)]
            | Same _, Start -> None
            | (Prev _ | Next _ | Replace _), (Start | Middle) | Same _, Middle ->
              Some (last, End))
          ~finishing_step:(function
            | None -> Done
            | Some result -> Yield (result, None))
    ;;

    include struct
      let%expect_test _ =
        let test ranges = print_s [%sexp (f ranges : t Sequence.t)] in
        let same = Range.Same [| "same", "same" |] in
        let not_same = Range.Next [| "new" |] in
        test [ same; same ];
        [%expect {| () |}];
        test [ same; not_same; same; same; not_same; same; same ];
        [%expect
          {|
        (((Same ((same same))) Start) ((Next (new)) Middle)
         ((Same ((same same) (same same))) Middle) ((Next (new)) Middle)
         ((Same ((same same) (same same))) End)) |}]
      ;;
    end
  end

  module Drop_or_keep : sig
    type t =
      | Drop of int
      (* drop n lines of extra context *)
      | Keep of string Range.t
    [@@deriving sexp_of]

    val f : context:int -> Merged_with_position.t Sequence.t -> t Sequence.t
  end = struct
    type t =
      | Drop of int
      | Keep of string Range.t
    [@@deriving sexp_of]

    let drop_from_start context lines =
      let extra_context = Array.length lines - context in
      if extra_context <= 0
      then Sequence.singleton (Keep (Same lines))
      else
        Sequence.of_list
          [ Drop extra_context
          ; Keep (Same (Array.sub ~pos:extra_context ~len:context lines))
          ]
    ;;

    let drop_from_end context lines =
      let extra_context = Array.length lines - context in
      if extra_context <= 0
      then Sequence.singleton (Keep (Same lines))
      else Sequence.singleton (Keep (Same (Array.sub ~pos:0 ~len:context lines)))
    ;;

    let drop_from_middle context lines =
      let extra_context = Array.length lines - (2 * context) in
      if extra_context <= 0
      then Sequence.singleton (Keep (Same lines))
      else (
        let start_next_context_at = Array.length lines - context in
        Sequence.of_list
          [ Keep (Same (Array.sub ~pos:0 ~len:context lines))
          ; Drop extra_context
          ; Keep (Same (Array.sub ~pos:start_next_context_at ~len:context lines))
          ])
    ;;

    let f ~context (ranges : Merged_with_position.t Sequence.t) =
      Sequence.bind ranges ~f:(fun (range, pos) ->
        match range with
        | Unified _ ->
          raise_s [%message "Unexpected Unified range." ~_:(range : string Range.t)]
        | Prev _ | Next _ | Replace _ -> Sequence.singleton (Keep range)
        | Same lines ->
          (match pos with
           | Start -> drop_from_start context lines
           | End -> drop_from_end context lines
           | Middle -> drop_from_middle context lines))
    ;;

    include struct
      let%expect_test _ =
        let test ranges =
          print_s [%sexp (Merged_with_position.f ranges |> f ~context:1 : t Sequence.t)]
        in
        let same = Range.Same [| "same", "same" |] in
        let not_same = Range.Next [| "new" |] in
        test [ same; same ];
        [%expect {| () |}];
        test
          [ same
          ; same
          ; not_same
          ; same
          ; same
          ; not_same
          ; same
          ; same
          ; same
          ; not_same
          ; same
          ; same
          ];
        [%expect
          {|
        ((Drop 1) (Keep (Same ((same same)))) (Keep (Next (new)))
         (Keep (Same ((same same) (same same)))) (Keep (Next (new)))
         (Keep (Same ((same same)))) (Drop 1) (Keep (Same ((same same))))
         (Keep (Next (new))) (Keep (Same ((same same))))) |}]
      ;;
    end
  end

  module Reconstruct_hunk : sig
    val f
      :  prev_start:int
      -> next_start:int
      -> Drop_or_keep.t Sequence.t
      -> string Hunk.t Sequence.t
  end = struct
    type t =
      { prev_start : int
      ; next_start : int
      ; ranges : string Range.t list
      }
    [@@deriving sexp_of]

    let to_hunk t =
      { Hunk.prev_start = t.prev_start
      ; prev_size = List.sum (module Int) t.ranges ~f:Range.prev_size
      ; next_start = t.next_start
      ; next_size = List.sum (module Int) t.ranges ~f:Range.next_size
      ; ranges = List.rev t.ranges
      }
    ;;

    let f ~prev_start ~next_start drop_or_keeps =
      Sequence.unfold_with_and_finish
        drop_or_keeps
        ~init:{ prev_start; next_start; ranges = [] }
        ~running_step:(fun t drop_or_keep ->
          match (drop_or_keep : Drop_or_keep.t) with
          | Keep range -> Skip { t with ranges = range :: t.ranges }
          | Drop n ->
            let hunk = to_hunk t in
            let t =
              { prev_start = t.prev_start + hunk.prev_size + n
              ; next_start = t.next_start + hunk.next_size + n
              ; ranges = []
              }
            in
            if List.is_empty (Hunk.ranges hunk) then Skip t else Yield (hunk, t))
        ~inner_finished:(fun t -> if List.is_empty t.ranges then None else Some t)
        ~finishing_step:(function
          | None -> Done
          | Some t -> Yield (to_hunk t, None))
    ;;
  end

  let enforce ~context hunk =
    Merged_with_position.f (Hunk.ranges hunk)
    |> Drop_or_keep.f ~context
    |> Reconstruct_hunk.f
         ~prev_start:(Hunk.prev_start hunk)
         ~next_start:(Hunk.next_start hunk)
    |> Sequence.to_list
  ;;
end

let apply hunks tolerance ~context =
  let equal = unstage (String_with_floats.close_enough tolerance) in
  do_tolerance ~equal hunks |> List.concat_map ~f:(Context_limit.enforce ~context)
;;
