open Core_kernel
open Angstrom

open Ast

open Omega_parser

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

let ignore p =
  p *> return ()

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
  Rewrite (atom, (match_template, rewrite_template))

let make_match_expression atom cases =
  Match (atom, cases)

(** rewrite <atom> { <atom> -> <atom> } *)
let rewrite_pattern_parser =
  lift3
    make_rewrite_expression
    (string Syntax.start_rewrite_pattern *> spaces *> atom_parser () <* spaces <* char '{' <* spaces)
    (antecedent_parser ~reserved:[" ->"] () <* spaces <* string Syntax.arrow <* spaces)
    (spaces *> rewrite_template_parser <* spaces <* char '}' <* spaces)

(** <atom> -> atom [, <expr>], [,] *)
let match_arrow_parser expression_parser =
  both
    (antecedent_parser ~reserved:[" ->"] () <* spaces <* string Syntax.arrow <* spaces)
    (spaces *> sep_by (char ',') expression_parser <* spaces <* optional_trailing ',' <* spaces)

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
      choice
        [ match_pattern_parser expression_parser
        ; rewrite_pattern_parser
        ; operator_parser
        ; true'
        ; false'
        ; option_parser
        ])

(** where <expression> [,] *)
let rule_parser =
  spaces *> string Syntax.rule_prefix *>
  spaces1 *> sep_by1 (spaces *> char ',' <* spaces) expression_parser
  <* optional_trailing ','
  <* spaces

let create rule =
  match parse_string ~consume:All (rule_parser <* end_of_input) rule with
  | Ok rule -> Ok rule
  | Error error -> Or_error.error_string error
