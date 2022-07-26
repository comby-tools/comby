open Core
open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "nested_matches" =
  let rule = {|where nested|} in
  let format = `Lines in
  let source = {|
a{
   b{
      c{
        d{e}
       }
    }
 }

q{
 b{
  f{}
 }
}
|} in
  let match_template = {|:[[f]]{:[x]}|} in
  run_all_matches (module Alpha.Generic) ~format source ~rule match_template;
  [%expect_exact
    {|2:a{\n   b{\n      c{\n        d{e}\n       }\n    }\n }
3:b{\n      c{\n        d{e}\n       }\n    }
4:c{\n        d{e}\n       }
5:d{e}
10:q{\n b{\n  f{}\n }\n}
11:b{\n  f{}\n }
12:f{}
|}];
  run_all_matches (module Omega.Generic) ~format source ~rule match_template;
  [%expect_exact
    {|2:a{\n   b{\n      c{\n        d{e}\n       }\n    }\n }
3:b{\n      c{\n        d{e}\n       }\n    }
4:c{\n        d{e}\n       }
5:d{e}
10:q{\n b{\n  f{}\n }\n}
11:b{\n  f{}\n }
12:f{}
|}];
  let source = {|a(b(c(d(e))))|} in
  let match_template = {|:[[f]](:[x])|} in
  run_all_matches (module Alpha.Generic) ~format source ~rule match_template;
  [%expect_exact {|1:a(b(c(d(e))))
1:b(c(d(e)))
1:c(d(e))
1:d(e)
|}];
  run_all_matches (module Omega.Generic) ~format source ~rule match_template;
  [%expect_exact {|1:a(b(c(d(e))))
1:b(c(d(e)))
1:c(d(e))
1:d(e)
|}]
