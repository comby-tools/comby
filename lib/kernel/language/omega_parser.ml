open Core_kernel

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

let quoted_parser =
  (string {|"|}
   *> (many_till char_token_s (string {|"|})))
  |>> String.concat

let operator_parser =
  choice
    [ string Syntax.equal
    ; string Syntax.not_equal
    ]

let any_char_except ~reserved =
  List.fold reserved
    ~init:(return `OK)
    ~f:(fun acc reserved_sequence ->
        option `End_of_input
          (peek_string (String.length reserved_sequence)
           >>= fun s ->
           if String.equal s reserved_sequence then
             return `Reserved_sequence
           else
             acc))
  >>= function
  | `OK -> any_char
  | `End_of_input -> any_char
  | `Reserved_sequence -> fail "reserved sequence hit"

let value_parser ~reserved () =
  match reserved with
  | [] -> fail "no value allowed to scan here"
  | reserved -> many (any_char_except ~reserved)

let antecedent_parser ?(reserved = []) () =
  choice
    [ (quoted_parser >>= fun value -> return (String value))
    ; (value_parser ~reserved () >>= fun value -> return (String (String.of_char_list value)))
    ]

let atom_parser ?(reserved = []) () =
  choice
    [ (variable_parser >>= fun variable -> return (Variable variable))
    ; (quoted_parser >>= fun value -> return (String value))
    ; (value_parser ~reserved () >>= fun value -> return (String (String.of_char_list value)))
    ]

let rewrite_template_parser =
  quoted_parser >>= fun value -> return (RewriteTemplate value)
