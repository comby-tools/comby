open Core

open Match

let substitute template env =
  Environment.vars env
  |> List.fold ~init:(template, []) ~f:(fun (acc, vars) variable ->
      match Environment.lookup env variable with
      | Some value ->
        if Option.is_some (String.substr_index template  ~pattern:(":["^variable^"]")) then
          (String.substr_replace_all acc ~pattern:(":["^variable^"]") ~with_:value, variable::vars)
        else
          acc, vars
      | None -> acc, vars)

let of_match_context
    { range =
        { match_start = { offset = start_index; _ }
        ; match_end = { offset = end_index; _ } }
    ; _
    }
    ~source =
  let before_part =
    if start_index = 0 then
      ""
    else
      String.slice source 0 start_index
  in
  let after_part = String.slice source end_index (String.length source) in
  let hole_id = Uuid.(Fn.compose to_string create ()) in
  let rewrite_template = String.concat [before_part; ":["; hole_id;  "]"; after_part] in
  hole_id, rewrite_template

(* return the offset for holes (specified by variables) in a given match template *)
let get_offsets_for_holes rewrite_template variables =
  let sorted_variables =
    List.fold variables ~init:[] ~f:(fun acc variable ->
        match String.substr_index rewrite_template ~pattern:(":["^variable^"]") with
        | Some index ->
          (variable, index)::acc
        | None -> acc)
    |> List.sort ~compare:(fun (_, i1) (_, i2) -> i1 - i2)
    |> List.map ~f:fst
  in
  List.fold sorted_variables ~init:(rewrite_template, []) ~f:(fun (rewrite_template, acc) variable ->
      match String.substr_index rewrite_template ~pattern:(":["^variable^"]") with
      | Some index ->
        let rewrite_template =
          String.substr_replace_all rewrite_template ~pattern:(":["^variable^"]") ~with_:"" in
        rewrite_template, (variable, index)::acc
      | None -> rewrite_template, acc)
  |> snd

(* pretend we substituted vars in offsets with environment. return what the offsets are after *)
let get_offsets_after_substitution offsets environment =
  List.fold_right offsets ~init:([],0) ~f:(fun (var, offset) (acc, shift)  ->
      match Environment.lookup environment var with
      | None -> failwith "Expected var"
      | Some s ->
        let offset' = offset + shift in
        let shift = shift + String.length s in
        ((var, offset')::acc), shift)
  |> fst
