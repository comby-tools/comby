open Vangstrom
open Core_kernel
open Match
open Replacement

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let counter =
  let uuid_for_id_counter = ref 0 in
  fun () ->
    uuid_for_id_counter := !uuid_for_id_counter + 1;
    Format.sprintf "%d" !uuid_for_id_counter

let replacement_sentinel metasyntax =
  let open Types.Metasyntax in
  List.find_map metasyntax.syntax ~f:(function
    | Hole (Everything, Delimited (left, right)) ->
      let left = Option.value left ~default:"" in
      let right = Option.value right ~default:"" in
      Some (left, right)
    | Hole (Alphanum, Delimited (left, right)) ->
      let left = Option.value left ~default:"" in
      let right = Option.value right ~default:"" in
      Some (left, right)
    | Regex (left, _, right) -> Some (left, right)
    | _ -> None)
  |> function
  | Some v -> v
  | None ->
    failwith
      "A custom metasyntax must define syntax for an Everything or Alphanum hole or Regex to \
       customize rewriting"

(** Parse the first :[id(label)] label encountered in the template. *)
let parse_first_label ?(metasyntax = Metasyntax.default_metasyntax) template =
  let label =
    take_while (function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | _ -> false)
  in
  let left, right = replacement_sentinel metasyntax in
  let parser =
    many
    @@ choice
         [ lift3 (fun _ label _ -> Some label) (string (left ^ "id(")) label (string (")" ^ right))
         ; (any_char >>| fun _ -> None)
         ]
  in
  parse_string ~consume:All parser template
  |> function
  | Ok label -> List.find_map label ~f:ident
  | Error _ -> None

let substitute_fresh ?(metasyntax = Metasyntax.default_metasyntax) ?(fresh = counter) template =
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
        if String.(label <> "") then String.Table.add_exn label_table ~key:label ~data:id;
        id
    in
    let left, right = replacement_sentinel metasyntax in
    let pattern = left ^ "id(" ^ label ^ ")" ^ right in
    template_ref := String.substr_replace_first !template_ref ~pattern ~with_:id;
    current_label_ref := parse_first_label ~metasyntax !template_ref
  done;
  !template_ref

let substitute_in_rewrite_template
  ?fresh
  ?(external_handler = External.default_external)
  ?(metasyntax = Metasyntax.default_metasyntax)
  ?filepath
  template
  environment
  =
  let (module M) = Metasyntax.create metasyntax in
  let module External = struct
    let handler = external_handler
  end
  in
  let module Template_parser = Template.Make (M) (External) in
  (* FIXME factor out Template_parser *)
  let template = substitute_fresh ~metasyntax ?fresh template in
  let terms = Template_parser.parse template in
  let replacement_content, environment = Template_parser.substitute ?filepath terms environment in
  { replacement_content
  ; environment
  ; range = { match_start = { Location.default with offset = 0 }; match_end = Location.default }
  }

let substitute
  ?(metasyntax = Metasyntax.default_metasyntax)
  ?external_handler
  ?fresh
  ?filepath
  template
  env
  =
  let { replacement_content; _ } =
    substitute_in_rewrite_template ~metasyntax ?external_handler ?fresh ?filepath template env
  in
  replacement_content

let substitute_matches (matches : Match.t list) source replacements =
  if debug then
    Format.printf
      "Matches: %d | Replacements: %d@."
      (List.length matches)
      (List.length replacements);
  let rewritten_source, in_place_substitutions, _ =
    (* shift adjusts the difference of the matched part and the replacement part to the matched offsets *)
    List.fold2_exn
      matches
      replacements
      ~init:(source, [], 0)
      ~f:(fun (rolling_result, replacements, shift) { range; _ } ({ replacement_content; _ } as r)
         ->
      let start_index = range.match_start.offset + shift in
      let end_index = range.match_end.offset + shift in
      let before = if start_index = 0 then "" else String.slice rolling_result 0 start_index in
      let after = String.slice rolling_result end_index (String.length rolling_result) in
      let match_length = end_index - start_index in
      let difference = String.length replacement_content - match_length in
      let range =
        Range.
          { match_start = Location.{ default with offset = start_index }
          ; match_end = Location.{ default with offset = end_index + difference }
          }
      in
      let replacements = { r with range } :: replacements in
      String.concat [ before; replacement_content; after ], replacements, shift + difference)
  in
  { rewritten_source; in_place_substitutions }

(* FIXME: all the functors help nothing if we end up calling this without parameterizing by metasyntax, etc. *)
let all ?source ?metasyntax ?external_handler ?fresh ?filepath ~rewrite_template rev_matches
  : result option
  =
  Option.some_if (not (List.is_empty rev_matches))
  @@
  match source with
  (* in-place substitution *)
  | Some source ->
    rev_matches
    |> List.map ~f:(fun Match.{ environment; _ } ->
         substitute_in_rewrite_template
           ?filepath
           ?metasyntax
           ?external_handler
           ?fresh
           rewrite_template
           environment)
    |> substitute_matches rev_matches source
  (* no in place substitution, emit result separated by newlines *)
  | None ->
    let buf = Buffer.create 20 in
    List.iter rev_matches ~f:(fun m ->
      substitute_in_rewrite_template
        ?filepath
        ?metasyntax
        ?external_handler
        ?fresh
        rewrite_template
        m.environment
      |> fun { replacement_content; _ } ->
      Buffer.add_string buf replacement_content;
      Buffer.add_char buf '\n');
    { rewritten_source = Buffer.contents buf; in_place_substitutions = [] }
