open Matchers
open Match

(** if [fresh] is set, then substitute the pattern :[id()] starting at 1, and
    incrementing subsequent IDs. If [fresh] is unset, then by default substitute
    the pattern :[id()] starting at 1, and increment for each occurence of
    :[id()], left to right. *)
val substitute_fresh : ?metasyntax:Metasyntax.t -> ?fresh:(unit -> string) -> string -> string

(** substitute returns the result and variables substituted for *)
val substitute : ?metasyntax:Metasyntax.t -> ?fresh:(unit -> string) -> string -> Environment.t -> (string * string list)

val of_match_context : ?metasyntax:Metasyntax.t -> ?fresh:(unit -> string) -> Match.t -> source:string -> (string * string)

val get_offsets_for_holes : ?metasyntax:Metasyntax.t -> string -> string list -> (string * int) list

val get_offsets_after_substitution : (string * int) list -> Environment.t -> (string * int) list
