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
  let open Types.Metasyntax in
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
let parse_first_label ?(metasyntax = Metasyntax.default_metasyntax) template =
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

let substitute_fresh
    ?(metasyntax = Metasyntax.default_metasyntax)
    ?(fresh = counter)
    template =
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

let substitute ?(metasyntax = Metasyntax.default_metasyntax) ?fresh template env =
  let (module M) = Metasyntax.create metasyntax in
  let module Template = Template.Make(M) in
  let vars = Template.variables template in
  let template = substitute_fresh ~metasyntax ?fresh template in
  if debug then Format.printf "Template after substituting fresh: %s@." template;

  List.fold vars ~init:(template, []) ~f:(fun (acc, vars) { variable; pattern; _ } ->
      match Environment.lookup env variable with
      | Some value ->
        if Option.is_some (String.substr_index template ~pattern) then
          String.substr_replace_all acc ~pattern ~with_:value, variable::vars
        else
          acc, vars
      | None ->
        acc, vars)
