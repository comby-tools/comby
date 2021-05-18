open Core

open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "gosimple_s1000" =
  let source =
    {|
      select {
      case x := <-ch:
        fmt.Println(x)
      }
    |}
  in

  let match_template =
    {|
      select {
      case :[1] := :[2]:
        :[3]
      }
    |}
  in

  let rewrite_template =
    {|
      :[1] := :[2]
      :[3]
    |}
  in

  run (module Alpha.Go) source match_template rewrite_template;
  [%expect_exact {|
      x := <-ch
      fmt.Println(x)
    |}];

  run (module Omega.Go) source match_template rewrite_template;
  [%expect_exact {|
      x := <-ch
      fmt.Println(x)
    |}]


let%expect_test "gosimple_s1001" =
  let source =
    {|
      for i, x := range src {
        dst[i] = x
      }
    |}
  in

  let match_template =
    {|
      for :[index_define], :[src_element_define] := range :[src_array] {
        :[dst_array][:[index_use]] = :[src_element_use]
      }
    |}
  in

  let rewrite_template =
    {|
      copy(:[dst_array], :[src_array])
    |}
  in

  let rule = {|where :[index_define] == :[index_use], :[src_element_define] == :[src_element_use]|} in

  run (module Alpha.Go) ~rule source match_template rewrite_template;
  [%expect_exact {|
      copy(dst, src)
    |}];

  run (module Omega.Go) ~rule source match_template rewrite_template;
  [%expect_exact {|
      copy(dst, src)
    |}]

let%expect_test "gosimple_s1003" =
  let source =
    {|
      if strings.Index(x, y) != -1 { ignore }
    |}
  in

  let match_template =
    {|
      if strings.:[1](x, y) != -1 { :[_] }
    |}
  in

  let rewrite_template = {|:[1]|} in

  run (module Alpha.Go) source match_template rewrite_template;
  [%expect_exact {|Index|}];
  run (module Omega.Go) source match_template rewrite_template;
  [%expect_exact {|Index|}]
