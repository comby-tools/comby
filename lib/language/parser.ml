open Core

open MParser
open MParser_PCRE.Tokens

open Ast

let variable_parser s =
  (string Syntax.variable_left_delimiter
   >> (many (alphanum <|> char '_') |>> String.of_char_list)
      << string Syntax.variable_right_delimiter) s

let value_parser s =
  string_literal s

let operator_parser s =
  ((string Syntax.equal)
   <|> (string Syntax.not_equal)) s

let atom_parser s =
  ((variable_parser >>= fun variable -> return (Variable variable))
   <|> (value_parser >>= fun value -> return (String value))) s

let rewrite_template_parser s =
  (value_parser >>= fun value -> return (RewriteTemplate value)) s
