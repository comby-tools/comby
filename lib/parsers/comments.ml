open Core
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
let non_nested_comment_delimiters from until s =
  (between
     (string from)
     (string until)
     (anything_including_newlines ~until)
   |>> to_string from until
  ) s

(** a parser for /* ... */ style block comments. *)
let c_multiline s =
  non_nested_comment_delimiters "/*" "*/" s

let c_newline s =
  (string "//" >> anything_excluding_newlines ~until:"\n"
   |>> fun l -> "//"^(String.of_char_list l)) s

let python_newline s =
  (string "#" >> anything_excluding_newlines ~until:"\n"
   |>> fun l -> ("#"^String.of_char_list l)) s

let percentage_newline s =
  (string "%" >> anything_excluding_newlines ~until:"\n"
   |>> fun l -> ("%"^String.of_char_list l)) s


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
let skip_nested_comments_inner from until s =
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
  (comment_delimiters >>= fun _ ->
   return ()) s
