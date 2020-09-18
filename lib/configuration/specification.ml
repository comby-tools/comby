open Core
open Angstrom

open Language

type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }

let (|>>) p f =
  p >>= fun x -> return (f x)

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

let identifier_parser () =
  many (alphanum <|> char '_')
  |>> String.of_char_list

let single_hole_parser () =
  string ":[[" *> identifier_parser () <* string "]]" |>> fun _ -> None

let everything_hole_parser () =
  string ":[" *> identifier_parser () <* string "]" |>> fun _ -> None

let expression_hole_parser () =
  string ":[" *> identifier_parser () <* string ":e" <* string "]" |>> fun _ -> None

let non_space_hole_parser () =
  string ":[" *> identifier_parser () <* string ".]" |>> fun _ -> None

let line_hole_parser () =
  string ":[" *> identifier_parser () <* string "\\n]" |>> fun _ -> None

let blank_hole_parser () =
  string ":["
  *> many1 blank
  *> identifier_parser ()
  <* string "]"
  |>> fun _ -> None

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

let regex_body () =
  fix (fun expr ->
      (choice
         [ ((char '[' *> (many1 expr) <* char ']')
            |>> fun char_class -> Format.sprintf "[%s]" @@ String.concat char_class)
         ; (char '\\' *> any_char |>> fun c -> (Format.sprintf "\\%c" c))
         ; ((any_char_except ~reserved:["]"])) |>> Char.to_string
         ]
      ))

let regex_hole_parser () =
  string ":["
  *> identifier_parser ()
  *> char '~'
  *> (many1 @@ regex_body ()) >>= fun regex ->
  string "]" >>= fun _ -> return (Some (String.concat regex))

type extracted =
  | Regex of string
  | Constant of string

let extract : extracted list Angstrom.t =
  let hole =
    choice
      [ single_hole_parser ()
      ; everything_hole_parser ()
      ; expression_hole_parser ()
      ; non_space_hole_parser ()
      ; line_hole_parser ()
      ; blank_hole_parser ()
      ; regex_hole_parser ()
      ]
  in
  many @@ choice
    [ (hole >>= fun v -> return (Option.map v ~f:(fun v -> Regex v)))
    ; ((many1 @@ any_char_except ~reserved:[":["])) >>= fun c ->
      return (Some (Constant (String.of_char_list c)))
    ]
  >>= fun result -> return (List.filter_opt result)

let escape s =
  let rec aux chars =
    match chars with
    | [] -> []
    | x :: xs ->
      match x with
      | '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' | '[' | ']' | '{' | '}' | '^' | '$' as c ->
        '\\' :: c :: (aux xs)
      | c -> c :: (aux xs)
  in
  aux (String.to_list s)
  |> String.of_char_list

let to_regex { match_template; _ } =
  let state = Buffered.parse extract in
  let state = Buffered.feed state (`String match_template) in
  let extracted =
    match Buffered.feed state `Eof with
    | Buffered.Done (_, result) -> result
    | _ -> failwith "Could not parse template for ripgrep"
  in
  (* Escape regex metachars *)
  let extracted = List.map extracted ~f:(function | Constant s -> escape s | Regex s -> s) in
  (* Replace contiguous spaces with the regex \s+ *)
  let match_spaces = Str.regexp "[ \t\r\n]+" in
  let extracted = List.map extracted ~f:(fun part -> Str.global_replace match_spaces {|\s+|} part) in
  (* ?s is modifier metasyntax where . matches all chars including newlines. See
     regular-expressions.info/modifier.html *)
  Format.sprintf "(%s)" @@ String.concat extracted ~sep:")(?s:.)*?("

let create ?rewrite_template ?rule ~match_template () =
  { match_template; rule; rewrite_template }
