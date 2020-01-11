open Core

open Angstrom

open Ast

let (|>>) p f =
  p >>= fun x -> return (f x)

let alphanum =
  satisfy (function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' -> true
      | _ -> false)

let variable_parser =
  (string Syntax.variable_left_delimiter
   *> (many (alphanum <|> char '_') |>> String.of_char_list)
   <* string Syntax.variable_right_delimiter)

let escaped_char_s  =
  any_char

let char_token_s =
  (char '\\' *> escaped_char_s >>= fun c -> return (Format.sprintf {|\%c|} c))
  <|> (any_char |>> String.of_char)

let value_parser =
  (string {|"|}
   *> (many_till char_token_s (string {|"|})))
  |>> String.concat

let operator_parser =
  choice
    [ string Syntax.equal
    ; string Syntax.not_equal
    ]

let atom_parser =
  choice
    [ (variable_parser >>= fun variable -> return (Variable variable))
    ; (value_parser >>= fun value -> return (String value))
    ]

let rewrite_template_parser =
  value_parser >>= fun value -> return (RewriteTemplate value)
