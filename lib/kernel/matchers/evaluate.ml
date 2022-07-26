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

let merge_match_environments matches environment' =
  List.map matches ~f:(fun { environment; _ } -> Environment.merge environment environment')

let apply
  ?(substitute_in_place = true)
  ?(metasyntax = Metasyntax.default_metasyntax)
  ?(external_handler = External.default_external)
  ?filepath
  ~(match_all :
     ?configuration:Configuration.t
     -> ?filepath:string
     -> template:string
     -> source:string
     -> unit
     -> Match.t list)
  rule
  env
  =
  let (module Metasyntax) = Metasyntax.create metasyntax in
  let module External = struct
    let handler = external_handler
  end
  in
  let (module Template : Types.Template.S) = (module Template.Make (Metasyntax) (External)) in
  let match_configuration_of_syntax template =
    (* decide match configuration based on whether there are holes *)
    match Template.variables template with
    | [] -> Configuration.create ~match_kind:Exact ()
    | _ -> Configuration.create ~match_kind:Fuzzy ()
  in
  let rewrite_substitute template env =
    Rewrite.substitute ~metasyntax ~external_handler ?filepath template env
  in
  let substitute env v =
    match v with
    | Template t -> rewrite_substitute (Template.to_string t) env
    | String s -> s
  in
  (* accepts only one expression *)
  let rec eval env = function
    (* true *)
    | True -> true, Some env
    (* false *)
    | False -> false, Some env
    (* option *)
    | Option _ -> true, Some env
    (* ==, != *)
    | Equal (Template t, String value) | Equal (String value, Template t) ->
      let other = rewrite_substitute (Template.to_string t) env in
      let result = String.equal value other in
      result, Some env
    | Equal (String left, String right) ->
      let result = String.equal left right in
      result, Some env
    | Equal (Template left, Template right) ->
      let left = rewrite_substitute (Template.to_string left) env in
      let right = rewrite_substitute (Template.to_string right) env in
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
        let configuration = { configuration with substitute_in_place } in
        if debug then Format.printf "Running for template %s source %s@." template source;
        match_all ~configuration ~template ~source ()
        |> function
        | [] -> None
        | matches ->
          (* merge environments. overwrite behavior is undefined *)
          if debug then Format.printf "Matches: %a@." Match.pp (None, matches);
          let fold_matches (sat, out) { environment; _ } =
            let fold_cases (sat, out) predicate =
              if sat then (
                let env' = Environment.merge env environment in
                eval env' predicate)
              else
                sat, out
            in
            List.fold case_expression ~init:(sat, out) ~f:fold_cases
          in
          List.fold matches ~init:(true, None) ~f:fold_matches |> Option.some
      in
      List.find_map cases ~f:(fun (template, case_expression) -> evaluate template case_expression)
      |> Option.value_map ~f:ident ~default:(false, Some env)
    (* rewrite ... { ... } *)
    | Rewrite (Template t, (match_template, rewrite_template)) ->
      let rewrite_template = substitute env rewrite_template in
      let template = substitute env match_template in
      let source = rewrite_substitute (Template.to_string t) env in
      let configuration = Configuration.create ~match_kind:Fuzzy () in
      let configuration = { configuration with substitute_in_place } in
      let matches = match_all ?filepath ~configuration ~template ~source () in
      let source = if substitute_in_place then Some source else None in
      let result = Rewrite.all ~metasyntax ?filepath ?source ~rewrite_template matches in
      if Option.is_empty result then
        if substitute_in_place then
          (* rewrites are always sat for in-place. always unsat for newline-sep. *)
          true, Some env
        else
          false, Some env
      else (
        let Replacement.{ rewritten_source; _ } = Option.value_exn result in
        (* substitute for variables that are in the outside scope *)
        let rewritten_source = rewrite_substitute rewritten_source env in
        let variable =
          match t with
          | [ Types.Template.Hole { variable; _ } ] -> variable
          | _ -> failwith "Cannot substitute for this template"
        in
        let env = Environment.update env variable rewritten_source in
        true, Some env)
    | Rewrite _ -> failwith "TODO/Invalid: Have not decided whether rewrite \":[x]\" is useful."
  in
  List.fold rule ~init:(true, None) ~f:(fun (sat, out) predicate ->
    if sat then (
      let env = Option.value_map out ~f:(fun out -> Environment.merge out env) ~default:env in
      eval env predicate)
    else
      sat, out)
