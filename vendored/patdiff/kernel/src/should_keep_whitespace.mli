open! Core_kernel
open! Import

(** Heuristics for whitespace-sensitive inputs (e.g., if it looks like Python). *)

val for_diff : prev:Diff_input.t -> next:Diff_input.t -> bool

(** Like {!for_diff}, but inputs are passed as a pair of [(file_name, lines)] rather than
    {!Diff_input.t}. *)
val for_diff_array : prev:string * string array -> next:string * string array -> bool
