open Core_kernel

open MParser

let to_string from until between : string =
  from ^ (String.of_char_list between) ^ until

let anything_including_newlines ~until =
  (many
     (not_followed_by (string until) ""
      >>= fun () -> any_char_or_nl))

let anything_excluding_newlines ~until =
  (many
     (not_followed_by (string until) ""
      >>= fun () -> any_char))

(** a parser for comments with delimiters [from] and [until] that do not nest *)
let non_nested_comment from until s =
  (between
     (string from)
     (string until)
     (anything_including_newlines ~until)
   |>> to_string from until
  ) s

let until_newline start s =
  (string start >> anything_excluding_newlines ~until:"\n"
   |>> fun l -> start^(String.of_char_list l)) s

let any_newline comment_string s =
  (string comment_string >> anything_excluding_newlines ~until:"\n" |>> fun l -> (comment_string^String.of_char_list l)) s

let is_not p s =
  if is_ok (p s) then
    Empty_failed (unknown_error s)
  else
    match read_char s with
    | Some c ->
      Consumed_ok (c, advance_state s 1, No_error)
    | None ->
      Empty_failed (unknown_error s)

(** A nested comment parser *)
let nested_comment from until s =
  let reserved = skip ((string from) <|> (string until)) in
  let rec grammar s =
    ((comment_delimiters >>= fun string -> return string)
     <|>
     (is_not reserved >>= fun c -> return (Char.to_string c)))
      s

  and comment_delimiters s =
    (between
       (string from)
       (string until)
       ((many grammar) >>= fun result ->
        return (String.concat result)))
      s
  in
  (comment_delimiters |>> fun content ->
   from ^ content ^ until) s

(** a parser for, e.g., /* ... */ style block comments. Non-nested. *)
module Multiline = struct
  module type S = sig
    val left : string
    val right : string
  end

  module Make (M : S) = struct
    let comment s = non_nested_comment M.left M.right s
  end
end

module Until_newline = struct
  module type S = sig
    val start : string
  end

  module Make (M : S) = struct
    let comment s = until_newline M.start s
  end
end

module Nested_multiline = struct
  module type S = sig
    val left : string
    val right : string
  end

  module Make (M : S) = struct
    let comment s = nested_comment M.left M.right s
  end
end
