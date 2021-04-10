open Core_kernel

type atom =
  | Variable of string
  | String of string
[@@deriving sexp]

type antecedent = atom
[@@deriving sexp]

type expression =
  | True
  | False
  | Option of string
  | Equal of atom * atom
  | Not_equal of atom * atom
  | Match of atom * (antecedent * consequent) list
  | RewriteTemplate of string
  | Rewrite of atom * (antecedent * expression)
and consequent = expression list
[@@deriving sexp]

let (=) left right = Equal (left, right)

let (<>) left right = Not_equal (left, right)

type t = expression list
[@@deriving sexp]

module Script = struct
  module Specification = struct
    type t =
      { match_template : string
      ; rule : expression list option
      ; rewrite_template : string option
      }
    [@@deriving sexp]
  end

  type spec = Specification.t
  [@@deriving sexp]

  type op =
    | And
    | Or
    | Not
  [@@deriving sexp]

  type exp =
    | Exp of op * exp list
    | Spec of spec
  [@@deriving sexp]

  type t = exp list
  [@@deriving sexp]
end
