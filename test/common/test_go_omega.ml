open Core

open Match
open Rewriter

open Test_helpers

include Test_omega

let run ?(rule = "where true") source match_template rewrite_template =
  let rule = Language.Rule.create rule |> Or_error.ok_exn in
  Go.first ~configuration match_template source
  |> function
  | Ok ({environment; _ } as result) ->
    if Rule.(sat @@ apply rule environment) then
      Rewrite.all ~source ~rewrite_template [result]
      |> (fun x -> Option.value_exn x)
      |> (fun { rewritten_source; _ } -> rewritten_source)    |> print_string
    else
      assert false
  | Error _ ->
    print_string rewrite_template

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
  run source match_template rewrite_template;
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

  run ~rule source match_template rewrite_template;
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

  run source match_template rewrite_template;
  [%expect_exact {|Index|}]
