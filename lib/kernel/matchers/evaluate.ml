open Core_kernel

open Match
open Types.Ast

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

(* FIXME. Shouldn't need this. *)
module Template = Template.Make(Metasyntax.Default)

(* FIXME this logic should go in rewrite *)
let evaluate_substitute env atom kind =
  let open Option in
  let length_to_string n = Format.sprintf "%d" (String.length n) in
  match atom, kind with
  | String v, Value -> return v
  | String v, Length -> return (length_to_string v)
  | Template t, Value -> Some (Rewrite.substitute (Template.to_string t) env)
  | Template t, Length ->
    Rewrite.substitute (Template.to_string t) env
    |> length_to_string
    |> return
  | _ -> failwith "invalid"

let apply
    ?(substitute_in_place = true)
    ?(fresh = counter)
    ?metasyntax
    ~(match_all:(?configuration:Configuration.t -> template:string -> source:string -> unit -> Match.t list))
    predicates
    env =
  let open Option in

  let _ = fresh () in (* FIXME not needed any more *)

  (* accepts only one expression *)
  let rec rule_match env =
    function
    | True -> true, Some env
    | False -> false, Some env
    | Option _ -> true, Some env
    | Equal (Template t, String value)
    | Equal (String value, Template t) ->
      let other = Rewrite.substitute (Template.to_string t) env in
      let result = String.equal value other in
      result, Some env
    | Equal (String left, String right) ->
      let result = String.equal left right in
      result, Some env
    | Equal (Template left, Template right) ->
      let left = Rewrite.substitute (Template.to_string left) env in
      let right = Rewrite.substitute (Template.to_string right) env in
      let result = String.equal left right in
      result, Some env
    | Not_equal (left, right) ->
      let sat, env = rule_match env (Equal (left, right)) in
      not sat, env
    | Match (Template t, cases) ->
      if debug then Format.printf "ENV: %s@." (Environment.to_string env);
      let result =
        let source = Rewrite.substitute (Template.to_string t) env in
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
            | String template -> evaluate template case_expression
            | Template template -> evaluate (Rewrite.substitute (Template.to_string template) env) case_expression)
      in
      Option.value_map result ~f:ident ~default:(false, Some env)
    | Match (String source, cases) ->  (* FIXME DEDUPE *)
      let result =
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
            | String template -> evaluate template case_expression
            | Template template -> evaluate (Rewrite.substitute (Template.to_string template) env) case_expression)
      in
      Option.value_map result ~f:ident ~default:(false, Some env)

    | Rewrite (Template t, (match_template, Template rewrite_template)) ->
      let template =
        match match_template with
        | Template t -> Rewrite.substitute (Template.to_string t) env
        | String s -> s
      in
      let result =
        let source = Rewrite.substitute (Template.to_string t) env in
        let configuration = Configuration.create ~match_kind:Fuzzy () in
        let matches = match_all ~configuration ~template ~source () in
        let source = if substitute_in_place then Some source else None in
        let result = Rewrite.all ?metasyntax ?source ~rewrite_template:(Template.to_string rewrite_template) matches in
        match result with
        | Some { rewritten_source; _ } ->
          (* substitute for variables that are in the outside scope *)
          let rewritten_source = Rewrite.substitute ?metasyntax rewritten_source env in
          let variable =
            match t with
            | [ Types.Template.Hole { variable; _ } ] -> variable
            | _ -> failwith "Cannot substitute for this"
          in
          let env = Environment.update env variable rewritten_source in
          return (true, Some env)
        | None ->
          return (true, Some env)
      in
      Option.value_map result ~f:ident ~default:(false, Some env)
    | Rewrite (Template t, (match_template, String rewrite_template)) -> (* FIXME DEDUPE *)
      let template =
        match match_template with
        | Template t -> Rewrite.substitute (Template.to_string t) env
        | String s -> s
      in
      let result =
        let source = Rewrite.substitute (Template.to_string t) env in
        let configuration = Configuration.create ~match_kind:Fuzzy () in
        let matches = match_all ~configuration ~template ~source () in
        if debug then Format.printf "Running for matches. Template: %s Source: %s. Matches: %d@." template source (List.length matches);
        let source = if substitute_in_place then Some source else None in
        if debug then Format.printf "Rewrite rule for source: %S Rewrite to: %S matches: %a@." (Option.value_exn source) rewrite_template Match.pp (None, matches);
        let result = Rewrite.all ?metasyntax ?source ~rewrite_template matches in
        match result with
        | Some { rewritten_source; _ } ->
          (* substitute for variables that are in the outside scope *)
          let rewritten_source = Rewrite.substitute ?metasyntax rewritten_source env in
          let variable =
            match t with
            | [ Types.Template.Hole { variable; _ } ] -> variable
            | _ -> failwith "Cannot substitute for this"
          in
          let env = Environment.update env variable rewritten_source in
          return (true, Some env)
        | None ->
          return (true, Some env)
      in
      Option.value_map result ~f:ident ~default:(false, Some env)

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
