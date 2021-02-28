open Angstrom

open Ast
open Script

let spaces = many @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)
let spaces1 = many1 @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)

let chainl1 e op =
  let rec parse acc = (lift2 (fun f x -> f acc x) op e >>= parse) <|> return acc in
  e >>= fun init -> parse init

let parens_parser p = char '(' *> (p <|> return []) <* char ')'

let atom_parser =
  let parser =
    spaces
    *> string Syntax.rule_prefix
    *> spaces1
    *> sep_by1 (spaces *> char ',' <* spaces) Omega_rule.expression_parser
  in
  parser >>= fun rule ->
  return [(Atom (Specification.{ rule = Some rule; match_template = ""; rewrite_template = None }))]

let not_parser exp_parser = (string "NOT" <|> string "not") *> spaces *> exp_parser >>= fun exp -> return [Exp (Not, exp)]
let and_parser = spaces *> (string "AND" <|> string "and") *> spaces *> return (fun left right -> [Exp (And, left@right)])
let or_parser = spaces *> (string "OR" <|> string "or") *> spaces *> return (fun left right -> [Exp (Or, left@right)])

let exp_parser =
  fix (fun exp ->
      let exp_parser = fix (fun exp' -> parens_parser exp <|> not_parser exp' <|> atom_parser) in
      let and_parser = chainl1 exp_parser and_parser in
      let repeat_parser = chainl1 and_parser or_parser in
      many (spaces *> repeat_parser <* spaces) >>| List.concat)

let parser = exp_parser <* end_of_input

let parse script =
  parse_string ~consume:All parser script
  |> Result.get_ok

let to_string exp =
  Sexplib.Sexp.to_string_hum (Script.sexp_of_t exp)
