open Core_kernel
open Vangstrom

type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }
[@@deriving sexp]

let create ?rewrite_template ?rule ~match_template () =
  { match_template; rule; rewrite_template }

let identifier_parser () =
  many (Omega_parser_helper.alphanum <|> char '_')
  >>| String.of_char_list

let single_hole_parser () =
  string ":[[" *> identifier_parser () <* string "]]" >>| fun _ -> Some {|(\w+)|}

let everything_hole_parser () =
  string ":[" *> identifier_parser () <* string "]" >>| fun _ -> Some {|(\n|.)*?|}

let expression_hole_parser () =
  string ":[" *> identifier_parser () <* string ":e" <* string "]" >>| fun _ -> Some {|(\n|.)*?|}

let non_space_hole_parser () =
  string ":[" *> identifier_parser () <* string ".]" >>| fun _ -> Some {|([^ \t\s\r\n])+|}

let line_hole_parser () =
  string ":[" *> identifier_parser () <* string "\\n]" >>| fun _ -> Some {|(\n|.)*?|}

let blank_hole_parser () =
  string ":["
  *> many1 Omega_parser_helper.blank
  *> identifier_parser ()
  <* string "]"
  >>| fun _ -> Some {|(\ |\t|\s|\r|\n)+|}

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
            >>| fun char_class -> Format.sprintf "[%s]" @@ String.concat char_class)
         ; (char '\\' *> any_char >>| fun c -> (Format.sprintf "\\%c" c))
         ; ((any_char_except ~reserved:["]"])) >>| Char.to_string
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
  | Contiguous_whitespace of string
  | Non_space of string

let up_to p =
  many1 (not_followed_by p *> any_char)

let extract : extracted list Vangstrom.t =
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
    [ (hole >>| fun v -> Option.map v ~f:(fun v -> Regex v))
    ; (Omega_parser_helper.spaces1 >>| fun s -> Some (Contiguous_whitespace s))
    ; (lift
         (fun v -> Some (Non_space (String.of_char_list v)))
         (up_to (choice [hole *> return (); Omega_parser_helper.spaces1 *> return ()])))
    ]
  >>| fun result -> List.filter_opt result

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
  let extracted = parse_string ~consume:All extract match_template |> Result.ok_or_failwith in
  (* Escape regex metachars and replace contiguous spaces with the regex \s+. Holes become a general regex. *)
  let b = Buffer.create 10 in
  Buffer.add_string b "(";
  List.iter extracted ~f:(function
      | Regex s -> Buffer.add_string b s
      | Non_space s -> Buffer.add_string b (escape s)
      | Contiguous_whitespace _ -> Buffer.add_string b {|\s+|});
  Buffer.add_string b ")";
  Buffer.contents b
