open Angstrom
open Core_kernel

open Omega_parser_helper

type syntax =
  { variable: string (* E.g., x *)
  ; pattern: string (*E.g., the entire :[x] part *)
  ; offset : int
  }
[@@deriving sexp_of]

type extracted =
  | Hole of syntax
  | Constant of string
[@@deriving sexp_of]

module Make (Metasyntax : Types.Metasyntax.S) = struct

  let identifier () =
    choice @@ List.map ~f:char (String.to_list Metasyntax.identifier)

  let identifier () =
    many1 (identifier ()) >>| String.of_char_list

  let regex_expression suffix =
    many1 @@
    fix (fun expr ->
        choice
          [ lift (fun x -> Format.sprintf "[%s]" @@ String.concat x) (char '[' *> many1 expr <* char ']')
          ; lift (fun c -> Format.sprintf {|\%c|} c) (char '\\' *> any_char)
          ; lift String.of_char (Deprecate.any_char_except ~reserved:[suffix])
          ])

  let regex_body separator suffix =
    lift2
      (fun v e -> v,e)
      (identifier ())
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

  let hole_prefixes =
    List.filter_map Metasyntax.syntax ~f:(function
        | Hole (_, Delimited (Some left, _))
        | Regex (left, _, _) -> Some [left]
        | Hole (_, Reserved_identifiers l) -> Some l
        | _ -> None)
    |> List.concat

  (** Not smart enough: only looks for hole prefix to stop scanning constant,
      because there isn't a good 'not' parser *)
  let parse_template : extracted list Angstrom.t =
    let hole = choice hole_parsers in
    many @@ choice
      [ (pos >>= fun offset -> hole >>| fun (pattern, variable) -> Hole { pattern; variable; offset })
      ; (((many1 @@ Omega_parser_helper.Deprecate.any_char_except ~reserved:hole_prefixes)) >>| fun c -> Constant (String.of_char_list c))
      ; any_char >>| fun c -> Constant (Char.to_string c) (* accept anything as constant not accepted by attempting holes above *)
      ]

  let parse template =
    match parse_string ~consume:All parse_template template with
    | Ok result -> Some result
    | Error e -> failwith ("No rewrite template parse: "^e)

  let variables template =
    let open Option in
    Option.value
      ~default:[]
      (parse template
       >>| List.filter_map ~f:(function
           | Hole { pattern; variable; offset } -> Some { pattern; variable; offset }
           | _ -> None))
end
