open! Core_kernel
open! Import

(** Default amount of context shown around each change in the diff *)
val default_context : int

(** The following constants were all chosen empirically. *)

(** Default cutoff for line-level semantic cleanup.  Any match of [default_line_big_enough]
    or more will not be deleted, even if it's surrounded by large inserts and deletes.
    Raising this quantity can only decrease the number of matches, and lowering it
    can only increase the number of matches. *)
val default_line_big_enough : int

(** Analogous to {!default_line_big_enough}, but for word-level refinement *)
val default_word_big_enough : int

(** Governs the behavior of [split_for_readability].  We will only split ranges around
    matches of size greater than [too_short_to_split].  Note that this should always
    be at least 1, otherwise we will split on a single `Newline token.
    Raising this quantity will result in less ranges being split, and setting it to
    infinity is the same as passing in [~interleave:false]. *)
val too_short_to_split : int

val warn_if_no_trailing_newline_in_both_default : bool

type t = private
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
  }
[@@deriving compare, fields, sexp_of]

include Invariant.S with type t := t

(** Raises if [invariant t] fails. *)
val create_exn
  :  output:Output.t
  -> rules:Format.Rules.t
  -> float_tolerance:Percent.t option
  -> produce_unified_lines:bool
  -> unrefined:bool
  -> keep_ws:bool
  -> split_long_lines:bool
  -> interleave:bool
  -> assume_text:bool
  -> context:int
  -> line_big_enough:int
  -> word_big_enough:int
  -> shallow:bool
  -> quiet:bool
  -> double_check:bool
  -> mask_uniques:bool
  -> prev_alt:string option
  -> next_alt:string option
  -> location_style:Format.Location_style.t
  -> warn_if_no_trailing_newline_in_both:bool
  -> t

val override
  :  ?output:Output.t
  -> ?rules:Format.Rules.t
  -> ?float_tolerance:Percent.t option
  -> ?produce_unified_lines:bool
  -> ?unrefined:bool
  -> ?keep_ws:bool
  -> ?split_long_lines:bool
  -> ?interleave:bool
  -> ?assume_text:bool
  -> ?context:int
  -> ?line_big_enough:int
  -> ?word_big_enough:int
  -> ?shallow:bool
  -> ?quiet:bool
  -> ?double_check:bool
  -> ?mask_uniques:bool
  -> ?prev_alt:string option
  -> ?next_alt:string option
  -> ?location_style:Format.Location_style.t
  -> ?warn_if_no_trailing_newline_in_both:bool
  -> t
  -> t

val default : t

module Private : sig
  (* [t] is private, so the only way to construct a [t] with [ext_cmp = Some _] is using
     this function, which is deprecated. *)
  val with_ext_cmp : t -> ext_cmp:string option -> notify:(unit -> unit) -> t
  [@@deprecated "[since 2020-04] Dangerous and slated for deletion"]
end
