open Vangstrom
open Core_kernel

open Match
open Types.Template

module Make (Metasyntax : Types.Metasyntax.S) = struct

  let up_to p =
    many1 (not_followed_by p *> any_char)

  let character () =
    choice @@ List.map ~f:char (String.to_list Metasyntax.identifier)

  let identifier () =
    many1 @@ character () >>| String.of_char_list

  let attribute_to_kind = function
    | "value" -> Value
    | "length" -> Length
    | "type" -> Type
    | _ -> failwith "invalid attribute"

  let attribute_access () =
    char '.' *> choice [ string "length"; string "type" ] <* not_followed_by (Omega_parser_helper.alphanum)

  let regex_expression suffix =
    many1 @@
    fix (fun expr ->
        choice
          [ lift (fun x -> Format.sprintf "[%s]" @@ String.concat x) (char '[' *> many1 expr <* char ']')
          ; lift (fun c -> Format.sprintf {|\%c|} c) (char '\\' *> any_char)
          ; lift String.of_char_list (up_to (string suffix))
          ])

  let regex_body separator suffix =
    both
      (option "" (identifier ()))
      (char separator *> regex_expression suffix)

  (** Folds left to respect order of definitions in custom metasyntax for
      matching, where we attempt to parse in order. Note this is significant if a
      syntax like $X~regex should be tried before shortcircuiting on $X, in which
      case it should be defined _after_ the $X syntax (most general should be
      first). *)
  let hole_parsers =
    let optional d = Option.value d ~default:"" in
    List.fold ~init:[] Metasyntax.syntax ~f:(fun acc v ->
        let result =
          match v with
          | Hole (_, Delimited (left, right)) ->
            lift4
              (fun left v right kind ->
                 let dot_attribute = if String.(kind = "value") then "" else "."^kind in
                 Format.sprintf "%s%s%s%s" left v right dot_attribute, v, kind)
              (string (optional left))
              (identifier ())
              (string (optional right))
              (option "value" (attribute_access ()))
          | Hole (_, Reserved_identifiers l) ->
            lift2
              (fun v kind ->
                 let dot_attribute = if String.(kind = "value") then "" else "."^kind in
                 Format.sprintf "%s%s" v dot_attribute, v, kind)
              (choice (List.map l ~f:string))
              (option "value" (attribute_access ()))
          | Regex (left, separator, right) ->
            lift4
              (fun left (v, expr) right kind ->
                 let dot_attribute = if String.(kind = "value") then "" else "."^kind in
                 Format.sprintf "%s%s%c%s%s%s"
                   left v separator (String.concat expr) right dot_attribute, v, kind)
              (string left)
              (regex_body separator right)
              (string right)
              (option "value" (attribute_access ()))
        in
        result::acc)

  let parse_template =
    let hole = choice hole_parsers in
    many @@ choice
      [ (pos >>= fun offset -> hole >>| fun (pattern, variable, kind) ->
         Hole { pattern; variable; offset; kind = attribute_to_kind kind })
      ; ((up_to (choice hole_parsers)) >>| fun c -> Constant (String.of_char_list c))
      ]

  let parse template =
    match parse_string ~consume:All parse_template template with
    | Ok result -> result
    | Error e -> failwith ("No rewrite template parse: "^e)

  let variables template =
    parse template
    |> List.filter_map ~f:(function
        | Hole { pattern; variable; offset; kind } ->
          Some { pattern; variable; offset; kind }
        | _ -> None)

  let to_string template =
    let buf = Buffer.create 10 in
    List.iter template ~f:(function
        | Constant c -> Buffer.add_string buf c
        | Hole { pattern; _ } -> Buffer.add_string buf pattern);
    Buffer.contents buf

  let substitute_kind { variable; kind; _ } env =
    let open Option in
    let length_to_string n = Format.sprintf "%d" (String.length n) in
    match kind with
    | Value -> Environment.lookup env variable
    | Length -> Environment.lookup env variable >>| length_to_string
    | Type -> failwith "unimplemented"

  let substitute template environment =
    let replacement_content, environment', _ =
      List.fold template ~init:([], Environment.create (), 0) ~f:(fun (result, env, pos) -> function
          | Constant c -> c::result, env, pos + String.length c
          | Hole ({ variable; pattern; _ } as h) ->
            match substitute_kind h environment with
            | None -> pattern::result, env, pos + String.length variable
            | Some value ->
              let advance = pos + String.length value in
              let range =
                Range.
                  { match_start = Location.{ default with offset = pos }
                  ; match_end = Location.{ default with offset = advance }
                  }
              in
              (* FIXME: here we should probably use pattern, or hole. We don't
                 want to substitute var x for length value if it's used as :[x]
                 vs :[x].length in the same rewrite template. This will only
                 affect the replacement values, which won't clobber the actual
                 result. *)
              let env = Environment.add ~range env variable value in
              value::result, env, advance)
    in
    String.concat (List.rev replacement_content), environment'

  (** Currently dead code. Alternative to substitute that searches for hole
      patterns and uses substr_replace_all. Don't know if it's faster, need to
      test. Also appears to have a minor offset issue. *)
  let substitute' template environment =
    let vars =
      List.filter_map template ~f:(function
          | Hole { pattern; variable; offset; kind } -> Some { pattern; variable; offset; kind }
          | _ -> None)
    in
    let template_string = to_string template in
    let replacement_content, environment =
      List.fold vars ~init:(template_string, Environment.create ()) ~f:(fun (template, env) { variable; pattern; _ } ->
          match Environment.lookup environment variable with
          | None -> template, env
          | Some value ->
            match String.substr_index template_string ~pattern with
            | None -> template, env
            | Some offset ->
              let range =
                Range.
                  { match_start = Location.{ default with offset }
                  ; match_end = Location.{ default with offset = offset + String.length value }
                  }
              in
              let env = Environment.add ~range env variable value in
              String.substr_replace_all template ~pattern ~with_:value, env)
    in
    replacement_content, environment
end
