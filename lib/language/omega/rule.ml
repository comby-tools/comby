open Core
open Angstrom

open Match
open Rewriter

open Ast
open Parser

module Configuration = Matchers.Configuration

type t = Ast.t

type result = bool * environment option

let (|>>) p f =
  p >>= fun x -> return (f x)

let sat = fst

let result_env = snd

let match_configuration_of_syntax template =
  (* decide match configuration based on whether there are holes *)
  let antecedent_contains_hole_syntax case =
    String.is_substring case ~substring:Syntax.variable_left_delimiter
  in
  if antecedent_contains_hole_syntax template then
    Configuration.create ~match_kind:Fuzzy ()
  else
    Configuration.create ~match_kind:Exact ()

let merge_match_environments matches environment' =
  List.map matches ~f:(fun { environment; _ } ->
      Environment.merge environment environment')

type rewrite_context =
  { variable : string }

let rec apply
    ?(matcher = (module Matchers.Generic : Matchers.Matcher))
    ?(substitute_in_place = true)
    predicates
    env =
  let open Option in
  let module Matcher = (val matcher : Matchers.Matcher) in

  let equal_in_environment var value env =
    match Environment.lookup env var with
    | None -> false, Some env
    | Some var_value -> String.equal var_value value, Some env
  in
  (* accepts only one expression *)
  let rec rule_match ?(rewrite_context : rewrite_context option) env =
    function
    | True -> true, Some env
    | False -> false, Some env
    | Equal (Variable var, String value)
    | Equal (String value, Variable var) ->
      equal_in_environment var value env
    | Equal (String left, String right) ->
      String.equal left right, Some env
    | Equal (Variable left, Variable right) ->
      let result =
        Environment.lookup env left >>= fun left ->
        Environment.lookup env right >>= fun right ->
        return (String.equal left right)
      in
      Option.value result ~default:false, Some env
    | Not_equal (left, right) ->
      let sat, env = rule_match env (Equal (left, right)) in
      not sat, env
    | Match (Variable variable, cases) ->
      let result =
        Environment.lookup env variable >>= fun source ->
        List.find_map cases ~f:(fun (template, case_expression) ->
            match template with
            | String template ->
              begin
                let configuration = match_configuration_of_syntax template in
                Matcher.all ~configuration ~template ~source |> function
                | [] -> None
                | matches ->
                  (* merge environments. overwrite behavior is undefined *)
                  let fold_matches (sat, out) { environment; _ } =
                    let fold_cases (sat, out) predicate =
                      if sat then
                        let env' = Environment.merge env environment in
                        rule_match ?rewrite_context env' predicate
                      else
                        (sat, out)
                    in
                    List.fold case_expression ~init:(sat, out) ~f:fold_cases
                  in
                  List.fold matches ~init:(true, None) ~f:fold_matches
                  |> Option.some
              end
            | Variable _ ->
              failwith "| :[hole] is invalid. Maybe you meant to put quotes")
      in
      Option.value_map result ~f:ident ~default:(false, Some env)
    | Match (String template, cases) ->
      let source, _ = Rewriter.Rewrite_template.substitute template env in
      let fresh_var = Uuid_unix.(Fn.compose Uuid.to_string create ()) in
      let env = Environment.add env fresh_var source in
      rule_match env (Match (Variable fresh_var, cases))
    | RewriteTemplate rewrite_template ->
      begin
        match rewrite_context with
        | None -> false, None
        | Some { variable; _ } ->
          (* FIXME(RVT) assumes only contextual rewrite for now. *)
          let env =
            Rewrite_template.substitute rewrite_template env
            |> fst
            |> fun replacement' ->
            Environment.update env variable replacement'
            |> Option.some
          in
          true, env
      end
    | Rewrite (Variable variable, (match_template, rewrite_expression)) ->
      begin match rewrite_expression with
        | RewriteTemplate rewrite_template ->
          let template =
            match match_template with
            | Variable _ -> failwith "Invalid syntax in rewrite LHS"
            | String template -> template
          in
          let result =
            Environment.lookup env variable >>= fun source ->
            let configuration = Configuration.create ~match_kind:Fuzzy () in
            let matches = Matcher.all ~configuration ~template ~source in
            let source = if substitute_in_place then Some source else None in
            let result = Rewrite.all ?source ~rewrite_template matches in
            match result with
            | Some { rewritten_source; _ } ->
              (* substitute for variables that are in the outside scope *)
              let rewritten_source, _ = Rewrite_template.substitute rewritten_source env in
              let env = Environment.update env variable rewritten_source in
              return (true, Some env)
            | None ->
              return (true, Some env)
          in
          Option.value_map result ~f:ident ~default:(false, Some env)
        | _ -> failwith "Not implemented yet"
      end
    | Rewrite _ -> failwith "TODO/Invalid: Have not decided whether rewrite \":[x]\" is useful."
  in
  List.fold predicates ~init:(true, None) ~f:(fun (sat, out) predicate ->
      if sat then
        let env =
          Option.value_map out
            ~f:(fun out -> Environment.merge out env)
            ~default:env
        in
        rule_match env predicate
      else
        (sat, out))


let make_equality_expression operator left right =
  if String.equal operator Syntax.equal then
    return (Equal (left, right))
  else if
    String.equal operator Syntax.not_equal then
    return (Not_equal (left, right))
  else
    let message =
      Format.sprintf
        "Unhandled operator %s. Did you mean %s or %s?"
        operator
        Syntax.equal
        Syntax.not_equal in
    fail message

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let spaces =
  take_while is_whitespace >>= fun s ->
  return s

let spaces1 =
  satisfy is_whitespace >>= fun c ->
  take_while is_whitespace >>= fun s ->
  return (Format.sprintf "%c%s" c s)

let create rule =
  let operator_parser =
    spaces *> atom_parser >>= fun left ->
    spaces *> operator_parser >>= fun operator ->
    spaces *> atom_parser >>= fun right ->
    make_equality_expression operator left right <* spaces
  in
  let true' = spaces *> string Syntax.true' <* spaces |>> fun _ -> True in
  let false' = spaces *> string Syntax.false' <* spaces |>> fun _ -> False in
  let expression_parser =
    fix (fun expression_parser ->
        let match_pattern_parser =
          let case_parser =
            spaces *> string Syntax.pipe_operator *>
            spaces *> atom_parser <* spaces <* string Syntax.arrow <* spaces >>= fun antecedent ->
            spaces *> sep_by (char ',') expression_parser <* spaces |>> fun consequent ->
            antecedent, consequent
          in
          let pattern keyword =
            string keyword *> spaces *> atom_parser <* spaces <* char '{' <* spaces
            >>= fun atom ->
            many1 case_parser
            <* char '}' <* spaces
            >>= fun cases -> return (atom, cases)
          in
          pattern Syntax.start_match_pattern |>> fun (atom, cases) ->
          Match (atom, cases)
        in
        let rewrite_pattern_parser =
          string Syntax.start_rewrite_pattern *> spaces *> atom_parser <* spaces <* char '{' <* spaces
          >>= fun atom ->
          atom_parser <* spaces <* string Syntax.arrow <* spaces >>= fun match_template ->
          spaces *> rewrite_template_parser <* spaces <* char '}' <* spaces
          |>> fun rewrite_template ->
          Rewrite (atom, (match_template, rewrite_template))
        in
        choice
          [ match_pattern_parser
          ; rewrite_pattern_parser
          ; operator_parser
          ; true'
          ; false'
          ])
  in
  let rule_parser =
    spaces
    *> string Syntax.rule_prefix
    *> spaces1
    *> sep_by1 (spaces *> char ',' <* spaces) expression_parser
    <* end_of_input
  in
  match parse_string rule_parser rule with
  | Ok rule -> Or_error.return rule
  | Error error -> Or_error.error_string error
