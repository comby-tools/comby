open Core

open Language

type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }

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
  let word_pattern = {|\w+|} in
  let hole_patterns =
    [ {|(:\[|} ^ word_pattern ^ {|\])|}
    ; {|(:\[\[|} ^ word_pattern ^ {|\]\])|}
    ; {|(:\[|} ^ word_pattern ^ {|\.\])|}
    ; {|(:\[|} ^ word_pattern ^ {|\\n\])|}
    ; {|(:\[|} ^ {|[ ]+(|} ^ word_pattern ^ {|)|} ^ {|\])|}
    ]
  in
  let regexes = String.concat ~sep:"|" hole_patterns in
  let spaces = Re2.create_exn {|\s+|} in
  let m = Re2.split (Re2.create_exn regexes) match_template in
  (* Escape regex metachars *)
  let m = List.map m ~f:escape in
  (* Replace contiguous spaces with the regex \s+ *)
  let m = List.map m ~f:(fun split -> Re2.replace_exn ~f:(fun _ -> {|\s+|}) spaces split) in
  Format.sprintf "(%s)" @@ String.concat m ~sep:")(?s:.)*?("

let create ?rewrite_template ?rule ~match_template () =
  { match_template; rule; rewrite_template }
