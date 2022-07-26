open Core_kernel
open Vangstrom

let skip p = p *> return ()
let up_to p = many1 (not_followed_by p *> any_char)
let between left right p = left *> p <* right
let zero : 'a Vangstrom.t = fail ""
let cons x xs = x :: xs
let many_till p t = fix (fun m -> t *> return [] <|> lift2 cons p m)
let many1_till p t = lift2 cons p (many_till p t)
let ignore p = p *> return ()

let many_till_stop p t =
  let stop = ref false in
  let set_stop v = stop := v in
  let get_stop () = !stop in
  fix (fun m ->
    choice
      [ (t >>= fun _ -> return (set_stop true) >>= fun _ -> fail "stop")
      ; (return () >>= fun _ -> if get_stop () then return [] else lift2 cons p m)
      ])

let many1_till_stop p t =
  let stop = ref false in
  let set_stop v = stop := v in
  let get_stop () = !stop in
  (* one needs to fail if p isn't successful so that it doesn't consume and advance one char *)
  let one =
    choice
      [ (t >>= fun _ -> return (set_stop true) >>= fun _ -> fail "stop")
      ; (return () >>= fun _ -> if get_stop () then fail "stop" else p)
      ]
  in
  lift2 cons one (many_till_stop p t)

let alphanum =
  satisfy (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false)

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let blank = choice [ char ' '; char '\t' ]
let space1 = satisfy is_whitespace
let spaces = take_while is_whitespace >>= fun s -> return s

let spaces1 =
  satisfy is_whitespace
  >>= fun c -> take_while is_whitespace >>= fun s -> return (Format.sprintf "%c%s" c s)

let identifier_parser () = many (alphanum <|> char '_') >>| String.of_char_list
