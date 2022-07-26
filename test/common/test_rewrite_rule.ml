open Core
open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "rewrite_rule" =
  let source = {|int|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in
  let rule = {|
      where rewrite :[1] { "int" -> "expect" }
    |} in
  run (module Alpha.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|expect|}];
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|expect|}]

let%expect_test "sequenced_rewrite_rule" =
  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|{ :[a] : :[rest] }|} in
  let rewrite_template = {|{ :[a] : :[rest] }|} in
  let rule =
    {|
      where
      rewrite :[a] { "a" -> "qqq" },
      rewrite :[rest] { "{ b : { :[other] } }" -> "{ :[other] }" }
    |}
  in
  run (module Alpha.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|{ { qqq : { c : d } } }|}];
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|{ { qqq : { c : d } } }|}]

let%expect_test "rewrite_rule_for_list" =
  let source = {|[1, 2, 3, 4,]|} in
  let match_template = {|[:[contents]]|} in
  let rewrite_template = {|[:[contents]]|} in
  let rule = {|
      where rewrite :[contents] { ":[[x]]," -> ":[[x]];" }
    |} in
  run (module Alpha.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|[1; 2; 3; 4;]|}];
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|[1; 2; 3; 4;]|}]

let%expect_test "rewrite_rule_for_list_strip_last" =
  let source = {|[1, 2, 3, 4]|} in
  let match_template = {|[:[contents]]|} in
  let rewrite_template = {|[:[contents]]|} in
  let rule = {|
      where rewrite :[contents] { ":[x], " -> ":[x]; " }
    |} in
  run (module Alpha.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|[1; 2; 3; 4]|}];
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|[1; 2; 3; 4]|}]

let%expect_test "haskell_example" =
  let source = {|
     (concat
     [ "blah blah blah"
     , "blah"
     ])
|} in
  let match_template = {|(concat [:[contents]])|} in
  let rewrite_template = {|(:[contents])|} in
  let rule = {|
      where rewrite :[contents] { "," -> "++" }
    |} in
  run (module Alpha.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|
     ( "blah blah blah"
     ++ "blah"
     )
|}];
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|
     ( "blah blah blah"
     ++ "blah"
     )
|}]

let%expect_test "rewrite_freeform_antecedent_pattern" =
  let source = {|
     (concat
     [ "blah blah blah"
     , "blah"
     ])
|} in
  let match_template = {|:[contents]|} in
  let rewrite_template = {|(:[contents])|} in
  let rule = {|
      where rewrite :[contents] { concat [:[x]] -> "nice" }
    |} in
  run (module Alpha.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|(
     (nice)
)|}];
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|(
     (nice)
)|}]
