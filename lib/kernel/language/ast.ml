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
