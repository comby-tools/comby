type syntax =
  { variable : string
  ; pattern : string
  ; offset : int
  }
[@@deriving sexp_of]

type extracted =
  | Hole of syntax
  | Constant of string
[@@deriving sexp_of]

module Make : Metasyntax.S -> sig
    val parse : string -> extracted list
    val variables : string -> syntax list
  end
