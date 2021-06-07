open Core_kernel
open Vangstrom

open Types.Ast

module Make (Metasyntax : Types.Metasyntax.S) (External : Types.External.S) = struct

  module Parser = Rule.Make (Metasyntax) (External)

  type spec = Specification.t
  [@@deriving sexp]

  type op =
    | And
    | Or
    | Not
  [@@deriving sexp]

  type exp =
    | Exp of op * exp list
    | Spec of spec
  [@@deriving sexp]

  type t = exp list
  [@@deriving sexp]

  let ignore p =
    p *> return ()

  let spaces = many @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)
  let spaces1 = many1 @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)

  let optional s = option () (ignore @@ string s)

  let chainl1 e op =
    let rec parse acc = (lift2 (fun f x -> f acc x) op e >>= parse) <|> return acc in
    e >>= fun init -> parse init

  let parens p = char '(' *> (p <|> return []) <* char ')'


  let template_parser until =
    choice
      [ (lift Parser.to_atom Parser.quoted_parser)
      ; (lift (fun v -> Parser.to_atom (String.of_char_list v)) (Omega_parser_helper.many1_till any_char until))
      ]

  let spec =
    let match_rewrite_parser =
      both
        (spaces *> template_parser (spaces *> string "->"))
        (option None (spaces *> string Syntax.arrow *> spaces *> template_parser (spaces1 *> string "where" *> spaces1)  >>| fun v -> Some v))
    in
    match_rewrite_parser >>= fun (match_template_atom, rewrite_template_atom) ->
    (option None (spaces1 *> Parser.parse >>| fun x -> Some x)) >>= fun rule ->
    let match_template = Sexplib.Sexp.to_string_hum (sexp_of_atom match_template_atom) in
    let rewrite_template =
      match rewrite_template_atom with
      | Some rewrite_template_atom -> Some (Sexplib.Sexp.to_string_hum (sexp_of_atom rewrite_template_atom))
      | None -> None
    in
    return [(Spec (Specification.{ match_template; rule; rewrite_template }))]

  let unop syntax exp_parser =
    choice (List.map ~f:string syntax) *> spaces *> exp_parser >>| fun exp -> [Exp (Not, exp)]

  let binop syntax op =
    spaces *> choice (List.map ~f:string syntax) *> spaces *> return (fun left right -> [Exp (op, left@right)])

  let exp_parser =
    fix (fun exp ->
        let exp_parser = fix (fun exp' -> parens exp <|> unop ["NOT"; "not"] exp' <|> spec) in
        let and_parser = chainl1 exp_parser @@ binop ["AND"; "and"] And in
        let seq_parser = chainl1 and_parser @@ binop ["OR"; "or"] Or in
        sep_by1 (spaces *> string Syntax.separator <* spaces) seq_parser >>| List.concat)

  let parser =
    spaces *> optional Syntax.separator *>
    exp_parser <* optional Syntax.separator <* spaces <* end_of_input

  let parse script =
    parse_string ~consume:All parser script

  let to_string exp =
    Sexplib.Sexp.to_string_hum (sexp_of_t exp)

end
