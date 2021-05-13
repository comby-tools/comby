open Core_kernel
open Angstrom

open Types.Ast

module Template = Template.Make(Metasyntax.Default)

module Parser = struct

  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

  let spaces =
    take_while is_whitespace

  let spaces1 =
    satisfy is_whitespace *>
    take_while is_whitespace

  let alphanum =
    satisfy (function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> true
        | _ -> false)

  let to_atom s =
    match Template.parse s with
    | [] -> String ""
    | [ Constant c ] -> String c
    | t -> Template t

  let variable_parser =
    lift3 (fun _ v _ -> String.of_char_list v)
      (string Syntax.variable_left_delimiter)
      (many (alphanum <|> char '_'))
      (string Syntax.variable_right_delimiter)

  (** Interpret escape sequences inside quotes *)
  let char_token_s =
    (char '\\' *> any_char >>|
     function
     | 'r' -> Char.to_string '\r'
     | 'n' -> Char.to_string '\n'
     | 't' -> Char.to_string '\t'
     | '\\' -> Char.to_string '\\'
     | c -> Format.sprintf {|\%c|} c)
    <|> (lift String.of_char any_char)

  (** With escape sequences *)
  let quote s =
    lift2 (fun _ v -> String.concat v)
      (string s)
      (many_till char_token_s (string s))

  let raw s =
    lift2 (fun _ v -> String.of_char_list v)
      (string s)
      (many_till any_char (string s))

  let quoted_parser =
    choice ~failure_msg:"could not parse quoted value"
      [ quote {|"|}; quote {|'|}; raw {|`|} ]

  let map_special s =
    if String.is_prefix s ~prefix:"~" then
      Template (Template.parse (Format.sprintf ":[%s]" s))
    else if String.equal s "_" then
      Template (Template.parse ":[_]")
    else
      to_atom s

  let up_to p =
    many1 (not_followed_by p *> any_char)

  let antecedent_parser () =
    choice ~failure_msg:"could not parse LHS of ->"
      [ (lift to_atom quoted_parser)
      ; (lift (fun v -> map_special (String.of_char_list v)) (up_to (spaces *> string Syntax.arrow)))
      ]

  let value_to_open_brace () =
    choice
      [ (lift to_atom quoted_parser)
      ; (lift (fun v -> to_atom (String.of_char_list v)) (up_to (spaces *> char '{')))
      ]

  let value_to_comma () =
    choice
      [ (lift to_atom quoted_parser)
      ; (lift (fun v -> to_atom (String.of_char_list v)) (up_to (spaces *> char ',')))
      ]

  let rewrite_consequent_parser () =
    choice
      [ (lift to_atom quoted_parser)
      ; (lift (fun v -> to_atom (String.of_char_list v)) (up_to (spaces *> char '}')))
      ]

  let operator_parser =
    choice
      [ string Syntax.equal
      ; string Syntax.not_equal
      ]

  let make_equality_expression left operator right =
    if String.equal operator Syntax.equal then
      Equal (left, right)
    else
      Not_equal (left, right)

  let optional_trailing c = option () (skip (Char.equal c))

  let option_parser =
    lift (fun _ -> Option "nested") (spaces *> (string Syntax.option_nested) <* spaces)

  let true' = lift (fun _ -> True) (spaces *> string Syntax.true' <* spaces)

  let false' = lift (fun _ -> False) (spaces *> string Syntax.false' <* spaces)

  (** <atom> [==, !=] <atom> *)
  let compare_parser =
    lift3
      make_equality_expression
      (spaces *> value_to_comma ())
      (spaces *> operator_parser)
      (spaces *> value_to_comma ())
    <* spaces

  let make_rewrite_expression atom match_template rewrite_template =
    Rewrite (atom, (match_template, rewrite_template))

  let make_match_expression atom cases =
    Match (atom, cases)

  (** rewrite <atom> { <atom> -> <atom> } *)
  let rewrite_pattern_parser =
    lift3
      make_rewrite_expression
      (string Syntax.start_rewrite_pattern *> spaces *> value_to_open_brace () <* spaces)
      (antecedent_parser () <* spaces <* string Syntax.arrow <* spaces)
      (rewrite_consequent_parser ())

  (** <atom> -> atom [, <expr>], [,] *)
  let match_arrow_parser expression_parser =
    both
      (antecedent_parser () <* spaces <* string Syntax.arrow <* spaces)
      (sep_by (char ',') expression_parser <* spaces <* optional_trailing ',' <* spaces)

  (** [|] <match_arrow> *)
  let first_case_parser expression_parser =
    spaces *> option () (Omega_parser_helper.ignore @@ string Syntax.pipe_operator *> spaces) *>
    match_arrow_parser expression_parser

  (** | <match_arrow> *)
  let case_parser expression_parser =
    spaces *> string Syntax.pipe_operator *> spaces *>
    match_arrow_parser expression_parser

  (** [|] <match_arrow> | <match_arrow> *)
  let case_block expression_parser =
    first_case_parser expression_parser >>= fun case ->
    many (case_parser expression_parser) >>= fun cases ->
    return (case :: cases)

  (** match <atom> { <case_parser> } *)
  let match_pattern_parser expression_parser =
    string Syntax.start_match_pattern *> spaces *>
    lift2
      make_match_expression
      (value_to_open_brace () <* spaces)
      (case_block expression_parser <* char '}' <* spaces)

  let expression_parser =
    fix (fun expression_parser ->
        choice ~failure_msg:"could not parse expression"
          [ match_pattern_parser expression_parser
          ; rewrite_pattern_parser
          ; compare_parser
          ; true'
          ; false'
          ; option_parser
          ])

  (** where <expression> [,] *)
  let parse =
    spaces *> string Syntax.rule_prefix *>
    spaces1 *> sep_by1 (spaces *> char ',' <* spaces) expression_parser
    <* optional_trailing ','
    <* spaces

  let create rule =
    match parse_string ~consume:All (parse <* end_of_input) rule with
    | Ok rule -> Ok rule
    | Error error -> Or_error.error_string error
end

type t = Types.Rule.t
[@@deriving sexp]

type options =
  { nested : bool
  }

let options rule =
  List.fold rule ~init:{ nested = false } ~f:(fun acc -> function
      | Types.Ast.Option name when String.(name = Syntax.option_nested) -> { nested = true }
      | _ -> acc)
