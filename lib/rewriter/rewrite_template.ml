open Core

open Match

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let substitute_fresh template =
  let pattern = ":[id()]" in
  let template_ref = ref template in
  while Option.is_some (String.substr_index !template_ref ~pattern) do
    let uuid = Uuid_unix.(Fn.compose Uuid.to_string create ()) in
    let id = String.suffix uuid 12 in
    template_ref := String.substr_replace_first !template_ref ~pattern ~with_:id
  done;
  !template_ref

let substitute template env =
  let substitution_formats =
    [ ":[ ", "]"
    ; ":[", ".]"
    ; ":[", "\\n]"
    ; ":[[", "]]"
    ; ":[", "]"
    (* optional syntax *)
    ; ":[? ", "]"
    ; ":[ ?", "]"
    ; ":[?", ".]"
    ; ":[?", "\\n]"
    ; ":[[?", "]]"
    ; ":[?", "]"
    ]
  in
  let template = substitute_fresh template in
  Environment.vars env
  |> List.fold ~init:(template, []) ~f:(fun (acc, vars) variable ->
      match Environment.lookup env variable with
      | Some value ->
        List.find_map substitution_formats ~f:(fun (left,right) ->
            let pattern = left^variable^right in
            if Option.is_some (String.substr_index template ~pattern) then
              Some (String.substr_replace_all acc ~pattern ~with_:value, variable::vars)
            else
              None)
        |> Option.value ~default:(acc,vars)
      | None -> acc, vars)

let of_match_context
    { range =
        { match_start = { offset = start_index; _ }
        ; match_end = { offset = end_index; _ } }
    ; _
    }
    ~source =
  if debug then Format.printf "Start idx: %d@.End idx: %d@." start_index end_index;
  let before_part =
    if start_index = 0 then
      ""
    else
      String.slice source 0 start_index
  in
  let after_part = String.slice source end_index (String.length source) in
  let hole_id = Uuid_unix.(Fn.compose Uuid.to_string create ()) in
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
