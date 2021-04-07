open Core_kernel

open Angstrom

let (|>>) p f =
  p >>= fun x -> return (f x)

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

let between left right p =
  left *> p <* right

let to_string from until between : string =
  from ^ (String.of_char_list between) ^ until

let anything_including_newlines ~until =
  many (any_char_except ~reserved:[until])

let anything_excluding_newlines () =
  anything_including_newlines ~until:"\n"

let non_nested_comment from until =
  between
    (string from)
    (string until)
    (anything_including_newlines ~until)
  |>> to_string from until


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
  (string start *> anything_excluding_newlines ()
   |>> fun l -> start^(String.of_char_list l))

module Until_newline = struct
  module type S = sig
    val start : string
  end

  module Make (M : S) = struct
    let comment = until_newline M.start
  end
end
