open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()


let run ?(configuration = configuration) (module M : Matchers.Matcher) source match_template rewrite_template =
  M.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let%expect_test "parse_rust_apostrophe_ok" =
  let source = {|pub struct GlobBuilder<'a> {}|} in
  let match_template = {|{}|} in
  let rewrite_template = {|{} // success|} in

  run (module Matchers.Rust) source match_template rewrite_template;
  [%expect_exact {|pub struct GlobBuilder<'a> {} // success|}]

let%expect_test "parse_ocaml_apostrophe_ok" =
  let source = {|type 'a t = Poly of 'a | Int of int |} in
  let match_template = {|type :[v] t = :[_] Int of :[i]|} in
  let rewrite_template = {|:[v], :[i]|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|'a, int |}]
