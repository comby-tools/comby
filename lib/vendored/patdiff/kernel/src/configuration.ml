open! Core_kernel
open! Import

let default_context = 16

(* The following constants were all chosen empirically. *)
(* Default cutoff for line-level semantic cleanup.  Any match of [default_line_big_enough]
   or more will not be deleted, even if it's surrounded by large inserts and deletes.
   Raising this quantity can only decrease the number of matches, and lowering it
   can only increase the number of matches. *)
let default_line_big_enough = 3

(* Analogous to above, but for word-level refinement *)
let default_word_big_enough = 7

(* Governs the behavior of [split_for_readability].  We will only split ranges around
   matches of size greater than [too_short_to_split].  Note that this should always
   be at least 1, otherwise we will split on a single `Newline token.
   Raising this quantity will result in less ranges being split, and setting it to
   infinity is the same as passing in [~interleave:false]. *)
let too_short_to_split = 2
let warn_if_no_trailing_newline_in_both_default = true

type t =
  { output : Output.t
  ; rules : Format.Rules.t
  ; ext_cmp : string option
  ; float_tolerance : Percent.t option
  ; produce_unified_lines : bool
  ; unrefined : bool
  ; keep_ws : bool
  ; split_long_lines : bool
  ; interleave : bool
  ; assume_text : bool
  ; context : int
  ; line_big_enough : int
  ; word_big_enough : int
  ; shallow : bool
  ; quiet : bool
  ; double_check : bool
  ; mask_uniques : bool
  ; prev_alt : string option
  ; next_alt : string option
  ; location_style : Format.Location_style.t
  ; warn_if_no_trailing_newline_in_both : bool
                                          [@default warn_if_no_trailing_newline_in_both_default] [@sexp_drop_default.equal]
  }
[@@deriving compare, fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f field = Invariant.check_field t f field in
    Fields.iter
      ~output:
        (check (fun output ->
           if Output.implies_unrefined output
           then [%test_eq: bool] t.unrefined true ~message:"output implies unrefined"))
      ~rules:ignore
      ~ext_cmp:
        (check (fun ext_cmp ->
           Option.iter ext_cmp ~f:(fun (_ : string) ->
             [%test_eq: bool] t.unrefined true ~message:"ext_cmp implies unrefined")))
      ~float_tolerance:
        (check (fun float_tolerance ->
           if Option.is_some float_tolerance
           then
             [%test_eq: string option]
               t.ext_cmp
               None
               ~message:"ext_cmp and float_tolerance cannot both be some"))
      ~produce_unified_lines:ignore
      ~unrefined:ignore
      ~keep_ws:ignore
      ~interleave:ignore
      ~assume_text:ignore
      ~split_long_lines:
        ignore
      ~context:ignore
      ~line_big_enough:
        (check (fun line_big_enough ->
           [%test_pred: int]
             Int.is_positive
             line_big_enough
             ~message:"line_big_enough must be positive"))
      ~word_big_enough:
        (check (fun word_big_enough ->
           [%test_pred: int]
             Int.is_positive
             word_big_enough
             ~message:"word_big_enough must be positive"))
      ~shallow:ignore
      ~quiet:ignore
      ~double_check:ignore
      ~mask_uniques:ignore
      ~prev_alt:ignore
      ~next_alt:ignore
      ~location_style:ignore
      ~warn_if_no_trailing_newline_in_both:ignore)
;;

let create_exn
      ~output
      ~rules
      ~float_tolerance
      ~produce_unified_lines
      ~unrefined
      ~keep_ws
      ~split_long_lines
      ~interleave
      ~assume_text
      ~context
      ~line_big_enough
      ~word_big_enough
      ~shallow
      ~quiet
      ~double_check
      ~mask_uniques
      ~prev_alt
      ~next_alt
      ~location_style
      ~warn_if_no_trailing_newline_in_both
  =
  let t =
    { output
    ; rules
    ; ext_cmp = None
    ; float_tolerance
    ; produce_unified_lines
    ; unrefined
    ; keep_ws
    ; split_long_lines
    ; interleave
    ; assume_text
    ; context
    ; line_big_enough
    ; word_big_enough
    ; shallow
    ; quiet
    ; double_check
    ; mask_uniques
    ; prev_alt
    ; next_alt
    ; location_style
    ; warn_if_no_trailing_newline_in_both
    }
  in
  invariant t;
  t
;;

let override_internal
      ?output
      ?rules
      ?ext_cmp
      ?float_tolerance
      ?produce_unified_lines
      ?unrefined
      ?keep_ws
      ?split_long_lines
      ?interleave
      ?assume_text
      ?context
      ?line_big_enough
      ?word_big_enough
      ?shallow
      ?quiet
      ?double_check
      ?mask_uniques
      ?prev_alt
      ?next_alt
      ?location_style
      ?warn_if_no_trailing_newline_in_both
      t
  =
  let output = Option.value ~default:t.output output in
  let ext_cmp = Option.value ~default:t.ext_cmp ext_cmp in
  let unrefined =
    Option.value ~default:t.unrefined unrefined
    || is_some ext_cmp
    || Output.implies_unrefined output
  in
  let t =
    let value value field = Option.value value ~default:(Field.get field t) in
    Fields.map
      ~output:(const output)
      ~rules:(value rules)
      ~ext_cmp:(const ext_cmp)
      ~float_tolerance:(value float_tolerance)
      ~produce_unified_lines:(value produce_unified_lines)
      ~unrefined:(const unrefined)
      ~keep_ws:(value keep_ws)
      ~interleave:(value interleave)
      ~assume_text:(value assume_text)
      ~split_long_lines:(value split_long_lines)
      ~context:(value context)
      ~line_big_enough:(value line_big_enough)
      ~word_big_enough:(value word_big_enough)
      ~shallow:(value shallow)
      ~quiet:(value quiet)
      ~double_check:(value double_check)
      ~mask_uniques:(value mask_uniques)
      ~prev_alt:(value prev_alt)
      ~next_alt:(value next_alt)
      ~location_style:(value location_style)
      ~warn_if_no_trailing_newline_in_both:(value warn_if_no_trailing_newline_in_both)
  in
  invariant t;
  t
;;

let override
      ?output
      ?rules
      ?float_tolerance
      ?produce_unified_lines
      ?unrefined
      ?keep_ws
      ?split_long_lines
      ?interleave
      ?assume_text
      ?context
      ?line_big_enough
      ?word_big_enough
      ?shallow
      ?quiet
      ?double_check
      ?mask_uniques
      ?prev_alt
      ?next_alt
      ?location_style
      ?warn_if_no_trailing_newline_in_both
      t
  =
  override_internal
    ?output
    ?rules
    ~ext_cmp:None
    ?float_tolerance
    ?produce_unified_lines
    ?unrefined
    ?keep_ws
    ?split_long_lines
    ?interleave
    ?assume_text
    ?context
    ?line_big_enough
    ?word_big_enough
    ?shallow
    ?quiet
    ?double_check
    ?mask_uniques
    ?prev_alt
    ?next_alt
    ?location_style
    ?warn_if_no_trailing_newline_in_both
    t
;;

let default =
  { output = Ansi
  ; rules =
      { line_same =
          Format.Rule.create
            []
            ~pre:(Format.Rule.Affix.create " |" ~styles:[ Bg Bright_black; Fg Black ])
      ; line_prev =
          Format.Rule.create
            [ Fg Red ]
            ~pre:(Format.Rule.Affix.create "-|" ~styles:[ Bg Red; Fg Black ])
      ; line_next =
          Format.Rule.create
            [ Fg Green ]
            ~pre:(Format.Rule.Affix.create "+|" ~styles:[ Bg Green; Fg Black ])
      ; line_unified =
          Format.Rule.create
            []
            ~pre:(Format.Rule.Affix.create "!|" ~styles:[ Bg Yellow; Fg Black ])
      ; word_same_prev = Format.Rule.create [ Dim ]
      ; word_same_next = Format.Rule.blank
      ; word_same_unified = Format.Rule.blank
      ; word_prev = Format.Rule.create [ Fg Red ]
      ; word_next = Format.Rule.create [ Fg Green ]
      ; hunk =
          Format.Rule.create
            [ Bold ]
            ~pre:(Format.Rule.Affix.create "@|" ~styles:[ Bg Bright_black; Fg Black ])
            ~suf:
              (Format.Rule.Affix.create
                 " ============================================================")
      ; header_prev =
          Format.Rule.create
            [ Bold ]
            ~pre:(Format.Rule.Affix.create "------ " ~styles:[ Fg Red ])
      ; header_next =
          Format.Rule.create
            [ Bold ]
            ~pre:(Format.Rule.Affix.create "++++++ " ~styles:[ Fg Green ])
      }
  ; ext_cmp = None
  ; float_tolerance = None
  ; produce_unified_lines = true
  ; unrefined = false
  ; keep_ws = false
  ; split_long_lines = false
  ; interleave = true
  ; assume_text = false
  ; context = default_context
  ; line_big_enough = default_line_big_enough
  ; word_big_enough = default_word_big_enough
  ; shallow = false
  ; quiet = false
  ; double_check = false
  ; mask_uniques = false
  ; prev_alt = None
  ; next_alt = None
  ; location_style = Diff
  ; warn_if_no_trailing_newline_in_both = warn_if_no_trailing_newline_in_both_default
  }
;;

module Private = struct
  let with_ext_cmp t ~ext_cmp ~notify =
    if is_some ext_cmp then notify ();
    { t with ext_cmp }
  ;;
end
