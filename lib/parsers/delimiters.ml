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

let between p from until =
  string from >> p << string until
