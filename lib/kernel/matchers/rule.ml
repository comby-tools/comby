open Core_kernel
open Angstrom

module Ast = struct
  type atom =
    | Variable of string
    | String of string
  [@@deriving sexp]

  type antecedent = atom
  [@@deriving sexp]

  type kind =
    | Value
    | Length
    | Type
    | File
  [@@deriving sexp]

  type expression =
    | True
    | False
    | Option of string
    | Equal of atom * atom
    | Not_equal of atom * atom
    | Match of atom * (antecedent * consequent) list
    | Rewrite of atom * (antecedent * expression)
    | Substitute of atom * kind
  and consequent = expression list
  [@@deriving sexp]

  let (=) left right = Equal (left, right)

  let (<>) left right = Not_equal (left, right)
end

module Parser = struct
  open Ast

  let alphanum =
    satisfy (function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> true
        | _ -> false)

  let variable_parser =
    (string Syntax.variable_left_delimiter
     *> (many (alphanum <|> char '_') >>| String.of_char_list)
     <* string Syntax.variable_right_delimiter)

  (** Interpret escape sequences inside quotes *)
  let char_token_s =
    (char '\\' *> any_char >>|
     function
     | 'r' -> Char.to_string '\r'
     | 'n' -> Char.to_string '\n'
     | 't' -> Char.to_string '\t'
     | '\\' -> Char.to_string '\\'
     | c -> Format.sprintf {|\%c|} c)
    <|> (any_char >>| String.of_char)

  (** With escape sequences *)
  let quote s =
    (string s *> (many_till char_token_s (string s)))
    >>| String.concat

  let raw s =
    (string s *> (many_till any_char (string s)))
    >>| String.of_char_list

  let quoted_parser =
    choice ~failure_msg:"could not parse quoted value"
      [ quote {|"|}; quote {|'|}; raw {|`|} ]

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
      Variable (Format.sprintf ":[%s]" s)
    else if String.equal s "_" then
      Variable ":[_]"
    else
      String s

  let antecedent_parser ?(reserved = []) () =
    choice ~failure_msg:"could not parse LHS of ->"
      [ (quoted_parser >>| fun value -> String value)
      ; (value_parser ~reserved () >>| fun value -> map_special (String.of_char_list value))
      ]

  let atom_parser () =
    choice ~failure_msg:"could not parse atom"
      [ (variable_parser >>| fun variable -> Variable variable)
      ; (quoted_parser >>| fun value -> String value)
      ]

  let ignore p =
    p *> return ()

  let make_equality_expression left operator right =
    if String.equal operator Syntax.equal then
      Equal (left, right)
    else
      Not_equal (left, right)

  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

  let spaces =
    take_while is_whitespace

  let spaces1 =
    satisfy is_whitespace *>
    take_while is_whitespace *>
    return ()

  let optional_trailing c = option () (skip (Char.equal c))

  let option_parser = spaces *> string Syntax.option_nested <* spaces >>| fun _ -> Option "nested"

  let true' = lift (fun _ -> True) (spaces *> string Syntax.true' <* spaces)

  let false' = lift (fun _ -> False) (spaces *> string Syntax.false' <* spaces)

  (** <atom> [==, !=] <atom> *)
  let operator_parser =
    lift3
      make_equality_expression
      (spaces *> atom_parser ())
      (spaces *> operator_parser)
      (spaces *> atom_parser ())
    <* spaces

  let make_rewrite_expression atom match_template rewrite_template =
    Rewrite (atom, (match_template, Substitute (rewrite_template, Value)))

  let make_match_expression atom cases =
    Match (atom, cases)

  let rewrite_consequent_parser () =
    choice ~failure_msg:"could not parse atom on RHS of ->"
      [ (atom_parser ()) <* spaces <* char '}'
      ; (value_parser ~reserved:[" }"; "}"] () <* spaces <* char '}' >>| fun value -> String (String.of_char_list value))
      ]

  (** rewrite <atom> { <atom> -> <atom> } *)
  let rewrite_pattern_parser =
    lift3
      make_rewrite_expression
      (string Syntax.start_rewrite_pattern *> spaces *> atom_parser () <* spaces <* char '{' <* spaces)
      (antecedent_parser ~reserved:[" ->"; "->"] () <* spaces <* string Syntax.arrow <* spaces)
      (rewrite_consequent_parser ())

  (** <atom> -> atom [, <expr>], [,] *)
  let match_arrow_parser expression_parser =
    both
      (antecedent_parser ~reserved:[" ->"; "->"] () <* spaces <* string Syntax.arrow <* spaces)
      (sep_by (char ',') expression_parser <* spaces <* optional_trailing ',' <* spaces)

  (** [|] <match_arrow> *)
  let first_case_parser expression_parser =
    spaces *> option () (ignore @@ string Syntax.pipe_operator *> spaces) *>
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
      (atom_parser () <* spaces <* char '{' <* spaces)
      (case_block expression_parser <* char '}' <* spaces)

  let expression_parser =
    fix (fun expression_parser ->
        choice ~failure_msg:"could not parse expression"
          [ match_pattern_parser expression_parser
          ; rewrite_pattern_parser
          ; operator_parser
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

type t = Ast.expression list
[@@deriving sexp]

type options =
  { nested : bool
  }

let options rule =
  List.fold rule ~init:{ nested = false } ~f:(fun acc -> function
      | Ast.Option name when String.(name = Syntax.option_nested) -> { nested = true }
      | _ -> acc)
