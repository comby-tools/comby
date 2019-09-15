open Core

open Match
open Language

let infer_equality_constraints environment =
  let vars = Environment.vars environment in
  List.fold vars ~init:[] ~f:(fun acc var ->
      if String.is_prefix var ~prefix:"equal~" then
        match String.split var ~on:'~' with
        | _equal :: target :: _uuid ->
          let expression = Language.Ast.Equal (Variable var, Variable target) in
          expression::acc
        | _ -> acc
      else
        acc)

let apply_rule ?(newline_separated = false) matcher rule matches =
  let open Option in
  match rule with
  | "" -> matches
  | rule ->
    let rule = Rule.create rule |> Or_error.ok_exn in
    List.filter_map matches ~f:(fun ({ environment; _ } as matched) ->
        let rule = rule @ infer_equality_constraints environment in
        let sat, env =  Rule.apply ~newline_separated ~matcher rule environment in
        (if sat then env else None)
        >>| fun environment -> { matched with environment })

let run
    ((module Matcher : Matchers.Matcher) as matcher)
    ?newline_separated configuration template source rule =
  let matches = Matcher.all ~configuration ~template ~source in
  apply_rule ?newline_separated matcher rule matches
