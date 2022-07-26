open Core
open Comby_kernel
open Matchers
open Test_helpers

let%expect_test "implicit_equals" =
  let source = "(fun i -> j x) (fun x -> x x)" in
  let match_template = "fun :[[a]] -> :[[a]] :[[a]]" in
  run_all_matches (module Alpha.Generic) source match_template;
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":16,"line":1,"column":17},"end":{"offset":28,"line":1,"column":29}},"environment":[{"variable":"a","value":"x","range":{"start":{"offset":20,"line":1,"column":21},"end":{"offset":21,"line":1,"column":22}}},{"variable":"a_equal_!@#$000000000003","value":"x","range":{"start":{"offset":25,"line":1,"column":26},"end":{"offset":26,"line":1,"column":27}}},{"variable":"a_equal_!@#$000000000004","value":"x","range":{"start":{"offset":27,"line":1,"column":28},"end":{"offset":28,"line":1,"column":29}}}],"matched":"fun x -> x x"}]}
|}];
  run_all_matches (module Omega.Generic) source match_template;
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":16,"line":1,"column":17},"end":{"offset":28,"line":1,"column":29}},"environment":[{"variable":"a","value":"x","range":{"start":{"offset":20,"line":1,"column":21},"end":{"offset":21,"line":1,"column":22}}},{"variable":"a_equal_!@#$000000000005","value":"x","range":{"start":{"offset":25,"line":1,"column":26},"end":{"offset":26,"line":1,"column":27}}},{"variable":"a_equal_!@#$000000000006","value":"x","range":{"start":{"offset":27,"line":1,"column":28},"end":{"offset":28,"line":1,"column":29}}}],"matched":"fun x -> x x"}]}
|}]
