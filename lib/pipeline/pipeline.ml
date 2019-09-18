open Core

open Match
open Language

let infer_equality_constraints environment =
  let vars = Environment.vars environment in
  List.fold vars ~init:[] ~f:(fun acc var ->
      if String.is_prefix var ~prefix:"equal_" then
        match String.split var ~on:'_' with
        | _equal :: target :: _uuid ->
          let expression = Language.Ast.Equal (Variable var, Variable target) in
          expression::acc
        | _ -> acc
      else
        acc)

let apply_rule ?(substitute_in_place = true) matcher rule matches =
  let open Option in
  List.filter_map matches ~f:(fun ({ environment; _ } as matched) ->
      let rule = rule @ infer_equality_constraints environment in
      let sat, env =  Rule.apply ~substitute_in_place ~matcher rule environment in
      (if sat then env else None)
      >>| fun environment -> { matched with environment })

let run matcher ?substitute_in_place ?rule configuration template source =
  let module Matcher = (val matcher : Matchers.Matcher) in
  let matches = Matcher.all ~configuration ~template ~source in
  match rule with
  | Some rule -> apply_rule ?substitute_in_place matcher rule matches
  | None -> matches
