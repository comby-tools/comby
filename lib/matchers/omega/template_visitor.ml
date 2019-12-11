open Angstrom
open Core_kernel

open Types

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let user_defined_delimiters =
  Languages.C.Syntax.user_defined_delimiters

let skip_unit p =
  p >>| ignore

let reserved_delimiters =
  List.concat_map user_defined_delimiters ~f:(fun (from, until) -> [from; until])
  |> List.append [":["; "]"]
  |> List.append [":[["; "]]"]

let reserved =
  reserved_delimiters @ [" "; "\n"; "\t"; "\r"]
  |> List.sort ~compare:(fun v2 v1 ->
      String.length v1 - String.length v2)

let any_char_except ~reserved =
  List.fold reserved
    ~init:(return `OK)
    ~f:(fun acc reserved_sequence ->
        option `End_of_input
          (peek_string (String.length reserved_sequence)
           >>= fun s ->
           if s = reserved_sequence then
             return `Reserved_sequence
           else
             acc))
  >>= function
  | `OK -> any_char
  | `End_of_input -> any_char
  | `Reserved_sequence -> fail "reserved sequence hit"

let spaces1 =
  satisfy is_whitespace >>= fun c ->
  (* XXX use skip_while once everything works.
     we don't need the string *)
  take_while is_whitespace >>= fun s ->
  return (Format.sprintf "%c%s" c s)

let alphanum =
  satisfy (function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' -> true
      | _ -> false)

let identifier_parser () =
  many (alphanum <|> char '_')
  >>| String.of_char_list

let single_hole_parser () =
  string ":[[" *> identifier_parser () <* string "]]"

let greedy_hole_parser () =
  string ":[" *> identifier_parser () <* string "]"

let hole_parser sort dimension : Hole.t t =
  let open Hole in
  let hole_parser =
    match sort with
    | Alphanum -> single_hole_parser ()
    | Everything -> greedy_hole_parser ()
    | _ -> failwith "not implemented"
  in
  hole_parser >>| fun identifier -> { sort; identifier; dimension; optional = false }

class state = object
  (* val location *)
  (* val visited *)
end

class virtual syntax = object
  method virtual user_defined_delimiters : (string * string) list
  method virtual escapable_string_literals : Syntax.escapable_string_literals option
  method virtual raw_string_literals : (string * string) list
  method virtual comments : Syntax.comment_kind list
end

class virtual ['a] visitor = object(self)
  inherit state
  inherit syntax

  method enter_hole (_hole : hole) : 'a list = []
  method enter_delimiter (_left : string) (_right : string) (_body : 'a list) : 'a list = []
  method enter_spaces (_spaces : string) : 'a list = []
  method enter_other (_other : string) : 'a list = []
  method enter_toplevel (_matcher : 'a list) = []

  method private generate_parser : 'a list t =
    let spaces = spaces1 >>| self#enter_spaces in
    let hole_parser =
      choice
        [ hole_parser Alphanum Code
        ; hole_parser Everything Code
        ]
      >>| self#enter_hole in
    let other =
      many1 (any_char_except ~reserved)
      >>| String.of_char_list
      >>| self#enter_other
    in
    fix (fun generator : 'a list t ->
        (* body of nested will be visited recursively *)
        let nested =
          self#user_defined_delimiters
          |> List.map ~f:(fun (left, right) ->
              string left *> generator <* string right
              >>| self#enter_delimiter left right)
          |> choice
        in
        (many @@ choice
           [ hole_parser
           ; spaces
           ; nested
           ; other
           ] >>| List.concat))
    >>| self#enter_toplevel

  method run template : 'a list =
    let state = Buffered.parse self#generate_parser in
    let state = Buffered.feed state (`String template) in
    Buffered.feed state `Eof
    |> function
    | Buffered.Done (_, acc) -> acc
    | _ -> []
end

class ['a] mapper = object(self)
  inherit state

  method map_delimiter (left : string) (right : string) (p : 'a t) : 'a t =
    string left *> p <* string right

  method generate_parser : 'a t =
    fix (fun generator : 'a t ->
        let left = "(" in
        let right = ")" in
        let acc = self#map_delimiter left right generator in
        acc)

  method run (template : string) : 'a t =
    let state = Buffered.parse self#generate_parser in
    let state = Buffered.feed state (`String template) in
    Buffered.feed state `Eof
    |> function
    | Buffered.Done (_, p) -> return p
    | _ -> failwith "nope"
end

let fold visitor template =
  visitor#run template

let map _mapper _template =
  assert false
