open Types.Template

module Make : Metasyntax.S -> sig

    module Matching : sig
      val hole_parsers : (Types.Hole.sort * string Vangstrom.t) list
    end

    val parse : string -> t

    val variables : string -> syntax list

    val to_string : t -> string

    val substitute : ?filepath:string -> t -> Match.Environment.t -> (string * Match.Environment.t)
  end
