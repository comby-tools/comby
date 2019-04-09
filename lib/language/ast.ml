open Core

type atom =
  | Variable of string
  | String of string
[@@deriving sexp]

type antecedent = atom
[@@deriving sexp]

type expression =
  | True
  | False
  | Equal of atom * atom
  | Not_equal of atom * atom
  | Match of atom * (antecedent * consequent) list
  | RewriteTemplate of string
  | Rewrite of atom * (antecedent * consequent) list
  | Rewrite_old of atom * ((atom * atom * expression list) list)
and consequent = expression list
[@@deriving sexp]

let (=) left right = Equal (left, right)

let (<>) left right = Not_equal (left, right)

(* Semantics for rewrite_rule with a list more than one expression is
   undefined *)
type t = expression list
[@@deriving sexp]
