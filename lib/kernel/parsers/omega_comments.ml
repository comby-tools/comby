open Core_kernel
open Vangstrom

let ( |>> ) p f = p >>= fun x -> return (f x)

let any_char_except ~reserved =
  List.fold reserved ~init:(return `OK) ~f:(fun acc reserved_sequence ->
    option
      `End_of_input
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

let between left right p = left *> p <* right
let to_string from until between : string = from ^ String.of_char_list between ^ until
let anything_including_newlines ~until = many (any_char_except ~reserved:[ until ])
let anything_excluding_newlines () = anything_including_newlines ~until:"\n"

let non_nested_comment from until =
  between (string from) (string until) (anything_including_newlines ~until) |>> to_string from until

let nested_comment from until =
  let reserved = choice [ string from *> return (); string until *> return () ] in
  let grammar =
    fix (fun grammar ->
      let comment_delimiters =
        between
          (string from)
          (string until)
          (many grammar >>| fun result -> String.concat result)
      in
      let other = not_followed_by reserved *> any_char >>| Char.to_string in
      choice [ comment_delimiters; other ])
  in
  between (string from) (string until) (many grammar >>| fun result -> String.concat result)
  >>| fun content -> from ^ content ^ until

module Multiline = struct
  module type S = sig
    val left : string
    val right : string
  end

  module Make (M : S) = struct
    let comment = non_nested_comment M.left M.right
  end
end

(* Consumes the newline if we don't reintroduce it. This can be improved, we
   shouldn't need to reintroduce it.*)
let until_newline start =
  string start *> anything_excluding_newlines () |>> fun l -> start ^ String.of_char_list l

module Until_newline = struct
  module type S = sig
    val start : string
  end

  module Make (M : S) = struct
    let comment = until_newline M.start
  end
end

module Nested_multiline = struct
  module type S = sig
    val left : string
    val right : string
  end

  module Make (M : S) = struct
    let comment = nested_comment M.left M.right
  end
end
