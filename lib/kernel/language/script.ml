open Angstrom

open Ast
open Script

let optional_trailing c = option () (skip (Char.equal c))
let spaces = many @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)
let spaces1 = many1 @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)

let chainl1 e op =
  let rec parse acc = (lift2 (fun f x -> f acc x) op e >>= parse) <|> return acc in
  e >>= fun init -> parse init

let parens p = char '(' *> (p <|> return []) <* char ')'

let spec =
  let match_rewrite_parser =
    let open Omega_parser in
    both
      (spaces *> atom_parser ())
      (option None (spaces *> string Syntax.arrow *> spaces *> atom_parser () >>| fun x -> Some x))
  in
  match_rewrite_parser >>= fun (match_template_atom, rewrite_template_atom) ->
  (option None (spaces1 *> Omega_rule.rule_parser >>| fun x -> Some x)) >>= fun rule ->
  let match_template = Sexplib.Sexp.to_string_hum (sexp_of_atom match_template_atom) in
  let rewrite_template =
    match rewrite_template_atom with
    | Some rewrite_template_atom -> Some (Sexplib.Sexp.to_string_hum (sexp_of_atom rewrite_template_atom))
    | None -> None
  in
  return [(Spec (Specification.{ match_template; rule; rewrite_template }))]

let unop syntax exp_parser =
  choice (List.map string syntax) *> spaces *> exp_parser >>| fun exp -> [Exp (Not, exp)]

let binop syntax op =
  spaces *> choice (List.map string syntax) *> spaces *> return (fun left right -> [Exp (op, left@right)])

let exp_parser =
  fix (fun exp ->
      let exp_parser = fix (fun exp' -> parens exp <|> unop ["NOT"; "not"] exp' <|> spec) in
      let and_parser = chainl1 exp_parser @@ binop ["AND"; "and"] And in
      let seq_parser = chainl1 and_parser @@ binop ["OR"; "or"] Or in
      sep_by1 (spaces *> char ';' <* spaces) seq_parser <* optional_trailing ';' <* spaces >>| List.concat)

let parser = exp_parser <* end_of_input

let parse script =
  parse_string ~consume:All parser script

let to_string exp =
  Sexplib.Sexp.to_string_hum (Script.sexp_of_t exp)
