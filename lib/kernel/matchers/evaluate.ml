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

(* FIXME. Propagate this. *)
module Template = Template.Make(Metasyntax.Default)

let substitute env = function
  | Template t -> Rewrite.substitute (Template.to_string t) env
  | String s -> s

let apply
    ?(substitute_in_place = true)
    ?metasyntax
    ~(match_all:(?configuration:Configuration.t -> template:string -> source:string -> unit -> Match.t list))
    predicates
    env =

  (* accepts only one expression *)
  let rec eval env =
    function
    (* true *)
    | True -> true, Some env
    (* false *)
    | False -> false, Some env
    (* option *)
    | Option _ -> true, Some env
    (* ==, != *)
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
      let sat, env = eval env (Equal (left, right)) in
      not sat, env

    (* match ... { ... } *)
    | Match (source, cases) ->
      let source = substitute env source in
      let evaluate template case_expression =
        let template = substitute env template in
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
                eval env' predicate
              else
                (sat, out)
            in
            List.fold case_expression ~init:(sat, out) ~f:fold_cases
          in
          List.fold matches ~init:(true, None) ~f:fold_matches
          |> Option.some
      in
      List.find_map cases ~f:(fun (template, case_expression) -> evaluate template case_expression)
      |> Option.value_map ~f:ident ~default:(false, Some env)

    (* rewrite ... { ... } *)
    | Rewrite (Template t, (match_template, rewrite_template)) ->
      let rewrite_template = substitute env rewrite_template in
      let template = substitute env match_template in
      let source = Rewrite.substitute (Template.to_string t) env in
      let configuration = Configuration.create ~match_kind:Fuzzy () in
      let matches = match_all ~configuration ~template ~source () in
      let source = if substitute_in_place then Some source else None in
      let result = Rewrite.all ?metasyntax ?source ~rewrite_template matches in
      if Option.is_empty result then
        true, Some env (* rewrites are always sat. *)
      else
        let Replacement.{ rewritten_source; _ } = Option.value_exn result in
        (* substitute for variables that are in the outside scope *)
        let rewritten_source = Rewrite.substitute ?metasyntax rewritten_source env in
        let variable =
          match t with
          | [ Types.Template.Hole { variable; _ } ] -> variable
          | _ -> failwith "Cannot substitute for this template"
        in
        let env = Environment.update env variable rewritten_source in
        true, Some env

    | Rewrite _ -> failwith "TODO/Invalid: Have not decided whether rewrite \":[x]\" is useful."
  in

  List.fold predicates ~init:(true, None) ~f:(fun (sat, out) predicate ->
      if sat then
        let env =
          Option.value_map out
            ~f:(fun out -> Environment.merge out env)
            ~default:env
        in
        eval env predicate
      else
        (sat, out))
