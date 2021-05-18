open Match

(** if [fresh] is set, then substitute the pattern :[id()] starting at 1, and
    incrementing subsequent IDs. If [fresh] is unset, then by default substitute
    the pattern :[id()] starting at 1, and increment for each occurence of
    :[id()], left to right. *)
val substitute_fresh : ?metasyntax:Metasyntax.t -> ?fresh:(unit -> string) -> string -> string

(** substitute returns the result of substituting env in template *)
val substitute : ?metasyntax:Metasyntax.t -> ?fresh:(unit -> string) -> string -> Environment.t -> string

(** if [source] is given, substitute in-place. If not,
    emit result separated by newlines *)
val all
  :  ?source:string
  -> ?metasyntax:Types.Metasyntax.t
  -> ?fresh:(unit -> string)
  -> rewrite_template:string
  -> Match.t list
  -> Replacement.result option
