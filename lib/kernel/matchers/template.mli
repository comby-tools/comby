open Types.Template

module Make : Metasyntax.S -> sig
    val parse : string -> t

    val variables : string -> syntax list

    val to_string : t -> string

    val substitute : t -> Match.Environment.t -> (string * Match.Environment.t)
  end
