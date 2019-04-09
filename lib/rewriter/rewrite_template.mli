open Match

(** substitute returns the result and variables substituted for *)
val substitute : string -> Environment.t -> (string * string list)

val of_match_context : Match.t -> source:string -> (string * string)

val get_offsets_for_holes : string -> string list -> (string * int) list

val get_offsets_after_substitution : (string * int) list -> Environment.t -> (string * int) list
