open Angstrom
open Core_kernel

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
