type syntax =
  { variable : string
  ; pattern : string
  ; offset : int
  }
[@@deriving sexp_of]

type atom =
  | Hole of syntax
  | Constant of string
[@@deriving sexp_of]

type t = atom list
[@@deriving sexp_of]

module Make : Metasyntax.S -> sig
    val parse : string -> t

    val variables : string -> syntax list
  end
