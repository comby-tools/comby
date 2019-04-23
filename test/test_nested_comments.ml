open Core

open Matchers

let configuration = Configuration.create ~match_kind:Fuzzy ()

let print_matches matches =
  List.map matches ~f:(fun matched -> `String matched.Match.matched)
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

(* See https://stackoverflow.com/questions/6698039/nested-comments-in-c-c *)
let%expect_test "nested_multiline_ocaml" =
  let source = {|int nest = /*/*/ 0 */**/ 1;|} in
  let template = {|0 * 1|} in
  (* 0 is not commented out *)
  let matches_no_nesting = C.all ~configuration ~template ~source in
  print_matches matches_no_nesting;
  [%expect_exact {|[ "0 */**/ 1" ]|}];

  let matches_no_nesting = C_nested_comments.all ~configuration ~template ~source in
  (* 0 is commented out *)
  print_matches matches_no_nesting;
  [%expect_exact {|[]|}];
