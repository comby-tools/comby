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

let quote s =
  (string s *> (many_till char_token_s (string s)))
  |>> String.concat

let quoted_parser =
  choice [ quote {|"|}; quote {|'|}; quote {|`|} ]

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

let map_special s =
  if String.is_prefix s ~prefix:"~" then
    Variable (Format.sprintf ":[_%s]" s)
  else if String.equal s "_" then
    Variable ":[_]"
  else
    String s

let antecedent_parser ?(reserved = []) () =
  choice
    [ (quoted_parser >>| fun value -> String value)
    ; (value_parser ~reserved () >>| fun value -> map_special (String.of_char_list value))
    ]

let atom_parser ?(reserved = []) () =
  choice
    [ (variable_parser >>| fun variable -> Variable variable)
    ; (quoted_parser >>| fun value -> String value)
    ; (value_parser ~reserved () >>| fun value -> String (String.of_char_list value))
    ]

let rewrite_template_parser =
  quoted_parser >>| fun value -> RewriteTemplate value
