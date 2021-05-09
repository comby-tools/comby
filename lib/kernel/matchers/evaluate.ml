open Core_kernel

open Match
open Rule
open Ast

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

type result = bool * Match.environment option

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

let counter =
  let uuid_for_id_counter = ref 0 in
  fun () ->
    uuid_for_id_counter := !uuid_for_id_counter + 1;
    Format.sprintf "gu3ssme_%012d" !uuid_for_id_counter

let equal_in_environment var value env =
  match Environment.lookup env var with
  | None -> false, Some env
  | Some var_value -> String.equal var_value value, Some env


let evaluate_substitute env atom kind =
  let open Option in
  let to_string n = Format.sprintf "%d" (String.length n) in
  match atom, kind with
  | String v, Value -> return v
  | String v, Length -> return (to_string v)
  | Variable v, Value -> Environment.lookup env v
  | Variable v, Length ->
    Environment.lookup env v >>= fun value ->
    return (to_string value)
  | _ -> failwith "invalid"

let apply
    ?(substitute_in_place = true)
    ?(fresh = counter)
    ?metasyntax
    ~(match_all:(?configuration:Configuration.t -> template:string -> source:string -> unit -> Match.t list))
    predicates
    env =
  let open Option in

  (* accepts only one expression *)
  let rec rule_match env =
    function
    | True -> true, Some env
    | False -> false, Some env
    | Option _ -> true, Some env
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
      if debug then Format.printf "ENV: %s@." (Environment.to_string env);
      let result =
        Environment.lookup env variable >>= fun source ->
        let evaluate template case_expression =
          let configuration = match_configuration_of_syntax template in
          if debug then Format.printf "Running for template %s source %s@." template source;
          match_all ~configuration ~template ~source () |> function
          | [] ->
            None
          | matches ->
            (* merge environments. overwrite behavior is undefined *)
            if debug then Format.printf "Matches: %a@." Match.pp (None, matches);
            let fold_matches (sat, out) { environment; _ } =
              let fold_cases (sat, out) predicate =
                if sat then
                  let env' = Environment.merge env environment in
                  rule_match env' predicate
                else
                  (sat, out)
              in
              List.fold case_expression ~init:(sat, out) ~f:fold_cases
            in
            List.fold matches ~init:(true, None) ~f:fold_matches
            |> Option.some
        in
        List.find_map cases ~f:(fun (template, case_expression) ->
            match template with
            | String template
            | Variable template ->
              evaluate template case_expression)
      in
      Option.value_map result ~f:ident ~default:(false, Some env)
    | Match (String template, cases) ->
      let source, _ = Rewriter.Rewrite_template.substitute ?metasyntax template env in
      let fresh_var = fresh () in
      let env = Environment.add env fresh_var source in
      rule_match env (Match (Variable fresh_var, cases))

    | Rewrite (Variable variable, (match_template, rewrite_expression)) ->
      let template =
        match match_template with
        | Variable _ -> failwith "Unsupported: please quote variable on LHS"
        | String template -> template
      in
      begin match rewrite_expression with
        | Substitute (String rewrite_template, Value) ->
          let result =
            Environment.lookup env variable >>= fun source ->
            let configuration = Configuration.create ~match_kind:Fuzzy () in
            let matches = match_all ~configuration ~template ~source () in
            let source = if substitute_in_place then Some source else None in
            let result = Rewrite.all ?metasyntax ?source ~rewrite_template matches in
            match result with
            | Some { rewritten_source; _ } ->
              (* substitute for variables that are in the outside scope *)
              let rewritten_source, _ = Rewrite_template.substitute ?metasyntax rewritten_source env in
              let env = Environment.update env variable rewritten_source in
              return (true, Some env)
            | None ->
              return (true, Some env)
          in
          Option.value_map result ~f:ident ~default:(false, Some env)
        | _ -> failwith "Not implemented yet"
      end
    | Rewrite _ -> failwith "TODO/Invalid: Have not decided whether rewrite \":[x]\" is useful."
    | _ -> failwith "nop"
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
