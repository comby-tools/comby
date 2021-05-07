open Angstrom
open Core_kernel

open Match

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

type syntax =
  { variable: string
  ; pattern: string
  }
[@@deriving sexp_of]

type extracted =
  | Hole of syntax
  | Constant of string
[@@deriving sexp_of]

module Make (Metasyntax : Types.Metasyntax.S) = struct

  let alphanum =
    satisfy (function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> true
        | _ -> false)

  let blank =
    choice
      [ char ' '
      ; char '\t'
      ]

  let ignore p =
    p *> return ()

  let p = function
    | Some delim -> ignore @@ (string delim)
    | None -> return ()

  let any_char_except ~reserved =
    List.fold reserved
      ~init:(return `OK)
      ~f:(fun acc reserved_sequence ->
          option `End_of_input
            (peek_string (String.length reserved_sequence)
             >>= fun s ->
             if String.equal s reserved_sequence then
               return `Reserved_sequence
             else
               acc))
    >>= function
    | `OK -> any_char
    | `End_of_input -> any_char
    | `Reserved_sequence -> fail "reserved sequence hit"

  let identifier () =
    choice @@ List.map ~f:char (String.to_list Metasyntax.identifier)

  let identifier () =
    both
      (option false (char '?' >>| fun _ -> true))
      (many1 (identifier ()) >>| String.of_char_list)

  let regex_expression suffix =
    fix (fun expr ->
        choice
          [ lift (fun x -> Format.sprintf "[%s]" @@ String.concat x) (char '[' *> many1 expr <* char ']')
          ; lift (fun c -> Format.sprintf {|\%c|} c) (char '\\' *> any_char)
          ; lift String.of_char (any_char_except ~reserved:[suffix])
          ])

  let regex_body separator suffix =
    lift2
      (fun v e -> v, e)
      (identifier ())
      (char separator *> many1 (regex_expression suffix))

  let hole_parsers =
    (* Fold left to respect order of definitions in custom metasyntax for
       matching, where we attempt to parse in order. Note this is significant if
       a syntax like $X~regex should be tried before shortcircuiting on $X, in
       which case it should be defined _after_ the $X syntax (most general
       should be first). *)
    List.fold ~init:[] Metasyntax.syntax ~f:(fun acc v ->
        let v =
          match v with
          | Hole (_, Delimited (left, right)) ->
            p left *> identifier () <* p right >>|
            fun (o, v) ->
            Format.sprintf "%s%s%s%s" (Option.value left ~default:"") (if o then "?" else "") v (Option.value right ~default:""),
            v
          | Hole (_, Reserved_identifiers l) ->
            choice (List.map l ~f:string) >>| fun v -> v, v
          | Regex (left, separator, right) ->
            p (Some left) *> regex_body separator right <* p (Some right) >>|
            fun ((_, v), expr) ->
            (Format.sprintf "%s%s%c%s%s" left v separator (String.concat expr) right),
            v
        in
        v::acc)

  let hole_prefixes =
    List.map Metasyntax.syntax ~f:(function
        | Hole (_, Delimited (Some left, _))
        | Regex (left, _, _) -> Some [left]
        | Hole (_, Reserved_identifiers l) -> Some l
        | _ -> None)
    |> List.filter_opt
    |> List.concat

  (** Not smart enough: only looks for hole prefix to stop scanning constant,
      because there isn't a good 'not' parser *)
  let parse_template : extracted list Angstrom.t =
    let hole = choice hole_parsers in
    many @@ choice
      [ (hole >>| fun (pattern, variable) -> Hole { pattern; variable } )
      ; (((many1 @@ any_char_except ~reserved:hole_prefixes)) >>| fun c -> Constant (String.of_char_list c))
      ; any_char >>| fun c -> Constant (Char.to_string c) (* accept anything as constant not accepted by attempting holes above *)
      ]

  let parse template =
    match parse_string ~consume:All parse_template template with
    | Ok result -> Some result
    | Error e -> failwith ("No rewrite template parse: "^e)

  let variables template =
    parse template
    |> function
    | Some result ->
      List.filter_map result ~f:(function
          | Hole { pattern; variable } -> Some { pattern; variable }
          | _ -> None)
    | None ->
      []
end

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
  let module Template_parser = Make(M) in
  let vars = Template_parser.variables template in
  let template = substitute_fresh ~metasyntax ?fresh template in
  if debug then Format.printf "Template after substituting fresh: %s@." template;

  List.fold vars ~init:(template, []) ~f:(fun (acc, vars) { variable; pattern } ->
      match Environment.lookup env variable with
      | Some value ->
        if Option.is_some (String.substr_index template ~pattern) then
          String.substr_replace_all acc ~pattern ~with_:value, variable::vars
        else
          acc, vars
      | None ->
        acc, vars)

(** Uses metasyntax to substitute fresh variables in the match_context that
    will be replaced. It returns (id * rewrite_template) where id is the part
    that will be substituted with match_context, and rewrite_template is the
    source that's been templatized. *)
let of_match_context
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
  let left, right = replacement_sentinel Metasyntax.default_metasyntax in
  let rewrite_template = String.concat [before_part; left; hole_id; right; after_part] in
  hole_id, rewrite_template

(** return the offset for holes (specified by variables) in a given match template *)
let get_offsets_for_holes
    variables
    rewrite_template =
  let sorted_variables =
    List.fold variables ~init:[] ~f:(fun acc { variable; pattern } ->
        match String.substr_index rewrite_template ~pattern with
        | Some index -> ((variable, pattern), index)::acc
        | None -> acc)
    |> List.sort ~compare:(fun (_, i1) (_, i2) -> i1 - i2)
    |> List.map ~f:fst
  in
  List.fold sorted_variables ~init:(rewrite_template, []) ~f:(fun (rewrite_template, acc) (variable, pattern) ->
      match String.substr_index rewrite_template ~pattern with
      | Some index ->
        let rewrite_template =
          String.substr_replace_all rewrite_template ~pattern ~with_:"" in
        rewrite_template, (variable, index)::acc
      | None -> rewrite_template, acc)
  |> snd

(** pretend we substituted vars in offsets with environment. return what the offsets are after *)
let get_offsets_after_substitution offsets environment =
  if debug then Format.printf "Environment: %s@." @@ Match.Environment.to_string environment;
  List.fold_right offsets ~init:([],0 ) ~f:(fun (var, offset) (acc, shift)  ->
      match Environment.lookup environment var with
      | None -> acc, shift
      | Some s ->
        let offset' = offset + shift in
        let shift = shift + String.length s in
        ((var, offset')::acc), shift)
  |> fst
