open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run source match_template rewrite_template =
  Lua.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches"
  | results ->
    Rewrite.all ~source ~rewrite_template results
    |> function
    | None -> print_string "No rewrite"
    | Some { rewritten_source; _ } -> print_string rewritten_source

let%expect_test "preserves_comments" =
  let source = {|
 line -- read a line
derp    --[[ this
is
a multiline
comment --]] xx
asdf

    --[[
    print(10)         -- no action (comment)
    --]]

    ---[[
    print(10)         --> 10
    --]]

    --[[
    print(10)         -- no action (comment)
    --]]

    ---[[
    print(10)         --> 10
    --]]

|} in
  let match_template = {|:[[w]]|} in
  let rewrite_template = {|XXX|} in

  run source match_template rewrite_template;
  [%expect_exact {|
 XXX -- read a line
XXX    --[[ this
is
a multiline
comment --]] XXX
XXX

    --[[
    print(10)         -- no action (comment)
    --]]

    ---[[
    XXX(XXX)         --> 10
    --]]

    --[[
    print(10)         -- no action (comment)
    --]]

    ---[[
    XXX(XXX)         --> 10
    --]]

|}]


let%expect_test "nested_raw_literals_are_ignored" =
  let source = {|
 page = [[
    raw
     [[ nested raw literal ]]
literal
    ]]

|} in
  let match_template = {|:[[w]]|} in
  let rewrite_template = {|XXX|} in

  run source match_template rewrite_template;
  [%expect_exact {|
 XXX = [[
    raw
     [[ nested raw literal ]]
literal
    ]]

|}]

let%expect_test "raw_literals_are_ignored" =
  let source = {|
 page = [[
    <HTML>
    </HTML>
    ]]

|} in
  let match_template = {|:[[page]]|} in
  let rewrite_template = {|XXX|} in

  run source match_template rewrite_template;
  [%expect_exact {|
 XXX = [[
    <HTML>
    </HTML>
    ]]

|}]

(* This thinks that the match pattern is a raw string, but it shouldn't: *)
(* echo '[[ blah ]]' | ./comby -stdin -matcher .lua '[:[x]]' '??' *)
(* should match contetually on raw strings. it doesn't. also, holes don't know how to nested match contextually for raw literals. *)
let%expect_test "holes_match_contextually_inside_raw_literals" =
  let source = {|
 page = [[
    blah blah
blah
    ]]

|} in
  let match_template = {|[[:[x]]]|} in
  let rewrite_template = {|derp|} in

  run source match_template rewrite_template;
  [%expect_exact {||}]

(* should ignore the bodies. it doesn't. *)
(*
let%expect_test "holes_matching_in_square_brackets_ignore_raw_literals" =
  let source = {|
 page = [[
    <HTML>
    </HTML>
    ]]

|} in
  let match_template = {|[:[page]]|} in
  let rewrite_template = {|XXX|} in

  run source match_template rewrite_template;
  [%expect_exact {|
 XXX = [[
    <HTML>
    </HTML>
    ]]

[[
x
[[ derp ]]
y
    ]]

|}]
*)
