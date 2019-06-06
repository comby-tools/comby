open Core
open MParser

(** Significant, potentially nested delimiters. *)

let parens' p =
  char '(' >> p << char ')'

let braces' p =
  char '{' >> p << char '}'

let brackets' p =
  char '<' >> p << char '>'

let squares' p =
  char '[' >> p << char ']'

let between_ambiguous p (from : string list) until =
  (List.map from ~f:string |> choice)
  >> p << string until

let between_p p pp until =
  pp >> p << string until

let between p from until =
  string from >> p << string until
