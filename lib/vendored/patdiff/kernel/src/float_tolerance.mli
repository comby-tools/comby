open Core_kernel
open Import

(** [apply hunks tolerance ~context] converts diff ranges into context if the diff looks
    like floating point numbers that changed by less than [tolerance].  If necessary,
    narrows or splits the resulting hunks to limit context to [context] lines.

    Float tolerance must be applied before refinement. *)
val apply
  :  string Patience_diff.Hunks.t
  -> Percent.t
  -> context:int
  -> string Patience_diff.Hunks.t
