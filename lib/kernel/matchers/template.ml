open Vangstrom
open Core_kernel

open Types.Template

module Make (Metasyntax : Types.Metasyntax.S) = struct

  let up_to p =
    many1 (not_followed_by p *> any_char)

  let character () =
    choice @@ List.map ~f:char (String.to_list Metasyntax.identifier)

  let identifier () =
    many1 @@ character () >>| String.of_char_list

  let regex_expression suffix =
    many1 @@
    fix (fun expr ->
        choice
          [ lift (fun x -> Format.sprintf "[%s]" @@ String.concat x) (char '[' *> many1 expr <* char ']')
          ; lift (fun c -> Format.sprintf {|\%c|} c) (char '\\' *> any_char)
          ; lift String.of_char_list (up_to (string suffix))
          ])

  let regex_body separator suffix =
    lift2
      (fun v e -> v,e)
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
            lift3
              (fun left v right -> Format.sprintf "%s%s%s" left v right, v)
              (string (optional left))
              (identifier ())
              (string (optional right))
          | Hole (_, Reserved_identifiers l) ->
            choice (List.map l ~f:string) >>| fun v -> v, v
          | Regex (left, separator, right) ->
            lift3
              (fun left (v, expr) right -> Format.sprintf "%s%s%c%s%s" left v separator (String.concat expr) right, v)
              (string left)
              (regex_body separator right)
              (string right)
        in
        result::acc)

  (** Not smart enough: only looks for hole prefix to stop scanning constant,
      because there isn't a good 'not' parser *)
  let parse_template =
    let hole = choice hole_parsers in
    many @@ choice
      [ (pos >>= fun offset -> hole >>| fun (pattern, variable) -> Hole { pattern; variable; offset })
      ; ((up_to (choice hole_parsers)) >>| fun c -> Constant (String.of_char_list c))
      ]

  let parse template =
    match parse_string ~consume:All parse_template template with
    | Ok result -> result
    | Error e -> failwith ("No rewrite template parse: "^e)

  let variables template =
    parse template
    |> List.filter_map ~f:(function
        | Hole { pattern; variable; offset } -> Some { pattern; variable; offset }
        | _ -> None)

  let to_string template =
    let buf = Buffer.create 10 in
    List.iter template ~f:(function
        | Constant c -> Buffer.add_string buf c
        | Hole { pattern; _ } -> Buffer.add_string buf pattern);
    Buffer.contents buf
end
