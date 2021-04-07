open Angstrom
open Core_kernel

open Match

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let counter =
  let uuid_for_id_counter = ref 0 in
  fun () ->
    uuid_for_id_counter := !uuid_for_id_counter + 1;
    Format.sprintf "%d" !uuid_for_id_counter

let sub_counter =
  let uuid_for_sub_counter = ref 0 in
  fun () ->
    uuid_for_sub_counter := !uuid_for_sub_counter + 1;
    Format.sprintf "sub_%d" !uuid_for_sub_counter

let replacement_sentinel metasyntax =
  let open Matchers.Metasyntax in
  List.find_map metasyntax.syntax ~f:(function
      | Hole (Everything, Delimited (left, right)) ->
        let left = Option.value left ~default:"" in
        let right = Option.value right ~default:"" in
        Some (left, right)
      | Regex (left, _, right) ->
        Some (left, right)
      | _ -> None)
  |> function
  | Some v -> v
  | None -> failwith "A custom metasyntax must define syntax for an Everything hole or Regex to customize rewriting"

(** Parse the first :[id(label)] label encountered in the template. *)
let parse_first_label ?(metasyntax = Matchers.Metasyntax.default_metasyntax) template =
  let label = take_while (function | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false) in
  let left, right = replacement_sentinel metasyntax in
  let parser =
    many @@
    choice
      [ (string (left^"id(") *> label <* string (")"^right) >>= fun label -> return (Some label))
      ; any_char >>= fun _ -> return None
      ]
  in
  parse_string ~consume:All parser template
  |> function
  | Ok label -> List.find_map label ~f:ident
  | Error _ -> None

let substitute_fresh ?(metasyntax = Matchers.Metasyntax.default_metasyntax) ?(fresh = counter) template =
  let label_table = String.Table.create () in
  let template_ref = ref template in
  let current_label_ref = ref (parse_first_label ~metasyntax !template_ref) in
  while Option.is_some !current_label_ref do
    let label = Option.value_exn !current_label_ref in
    let id =
      match String.Table.find label_table label with
      | Some id -> id
      | None ->
        let id = fresh () in
        if String.(label <> "") then
          String.Table.add_exn label_table ~key:label ~data:id;
        id
    in
    let left, right = replacement_sentinel metasyntax in
    let pattern = left ^ "id(" ^ label ^ ")" ^ right in
    template_ref := String.substr_replace_first !template_ref ~pattern ~with_:id;
    current_label_ref := parse_first_label ~metasyntax !template_ref;
  done;
  !template_ref

let formats_of_metasyntax metasyntax =
  let open Matchers.Metasyntax in
  List.filter_map metasyntax ~f:(function
      | Hole (_, Delimited (left, right)) ->
        let left = Option.value left ~default:"" in
        let right = Option.value right ~default:"" in
        Some [(left, right); (left^"?", right)]
      | _ -> None)
  |> List.concat

let substitute ?(metasyntax = Matchers.Metasyntax.default_metasyntax) ?fresh template env =
  let substitution_formats = formats_of_metasyntax metasyntax.syntax in
  let template = substitute_fresh ~metasyntax ?fresh template in
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
    ?(metasyntax = Matchers.Metasyntax.default_metasyntax)
    ?(fresh = sub_counter)
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
  let hole_id = fresh () in
  let left, right = replacement_sentinel metasyntax in
  let rewrite_template = String.concat [before_part; left; hole_id; right; after_part] in
  hole_id, rewrite_template

(* return the offset for holes (specified by variables) in a given match template *)
let get_offsets_for_holes ?(metasyntax = Matchers.Metasyntax.default_metasyntax) rewrite_template variables =
  let left, right = replacement_sentinel metasyntax in
  let sorted_variables =
    List.fold variables ~init:[] ~f:(fun acc variable ->
        match String.substr_index rewrite_template ~pattern:(left^variable^right) with
        | Some index ->
          (variable, index)::acc
        | None -> acc)
    |> List.sort ~compare:(fun (_, i1) (_, i2) -> i1 - i2)
    |> List.map ~f:fst
  in
  List.fold sorted_variables ~init:(rewrite_template, []) ~f:(fun (rewrite_template, acc) variable ->
      match String.substr_index rewrite_template ~pattern:(left^variable^right) with
      | Some index ->
        let rewrite_template =
          String.substr_replace_all rewrite_template ~pattern:(left^variable^right) ~with_:"" in
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
