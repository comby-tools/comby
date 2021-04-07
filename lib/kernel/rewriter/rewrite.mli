(** if [source] is given, substitute in-place. If not,
    emit result separated by newlines *)
val all
  :  ?source:string
  -> ?metasyntax:Matchers.Metasyntax.t
  -> ?fresh:(unit -> string)
  -> rewrite_template:string
  -> Match.t list
  -> Replacement.result option
