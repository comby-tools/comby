open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let all ?(configuration = configuration) template source =
  C.all ~configuration ~template ~source

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let%expect_test "rewrite_comments_1" =
  let template = "replace this :[1] end" in
  let source = "/* don't replace this () end */ do replace this () end" in
  let rewrite_template = "X" in

  all template source
  |> (fun matches ->
      Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "/* don't replace this () end */ do X"]

let%expect_test "rewrite_comments_2" =
  let template =
    {|
      if (:[1]) { :[2] }
    |}
  in

  let source =
    {|
      /* if (fake_condition_body_must_be_non_empty) { fake_body; } */
      // if (fake_condition_body_must_be_non_empty) { fake_body; }
      if (real_condition_body_must_be_empty) {
        int i;
        int j;
      }
    |}
  in

  let rewrite_template =
    {|
      if (:[1]) {}
    |}
  in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact
    {|
      /* if (fake_condition_body_must_be_non_empty) { fake_body; } */
      if (real_condition_body_must_be_empty) {}
    |}]

let%expect_test "capture_comments" =
  let template = {|if (:[1]) { :[2] }|} in
  let source = {|if (true) { /* some comment */ console.log(z); }|} in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": 1, "column": 1 },
      "end": { "offset": 48, "line": 1, "column": 49 }
    },
    "environment": [
      {
        "variable": "1",
        "value": "true",
        "range": {
          "start": { "offset": 4, "line": 1, "column": 5 },
          "end": { "offset": 8, "line": 1, "column": 9 }
        }
      },
      {
        "variable": "2",
        "value": "console.log(z);",
        "range": {
          "start": { "offset": 31, "line": 1, "column": 32 },
          "end": { "offset": 46, "line": 1, "column": 47 }
        }
      }
    ],
    "matched": "if (true) { /* some comment */ console.log(z); }"
  }
]|}]

let%expect_test "single_quote_in_comment" =
  let template =
    {| {:[1]} |}
  in

  let source =
    {|
       /*'*/
       {test}
    |}
  in

  let rewrite_template =
    {|
      {:[1]}
    |}
  in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact
    {|
      {test}
    |}]

let%expect_test "single_quote_in_comment" =
  let template =
    {| {:[1]} |}
  in

  let source =
    {|
       {
         a = 1;
         /* Events with mask == AE_NONE are not set. So let's initiaize the
          * vector with it. */
         for (i = 0; i < setsize; i++)
       }
    |}
  in

  let rewrite_template =
    {|
      {:[1]}
    |}
  in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact
    {|
      {
         a = 1;
         /* Events with mask == AE_NONE are not set. So let's initiaize the
          * vector with it. */
         for (i = 0; i < setsize; i++)
       }
    |}]

let%expect_test "single_quote_in_comment" =
  let template =
    {| {:[1]} |}
  in

  let source =
    {|
       {
         a = 1;
         /* ' */
         for (i = 0; i < setsize; i++)
       }
    |}
  in

  let rewrite_template =
    {|
      {:[1]}
    |}
  in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact
    {|
      {
         a = 1;
         /* ' */
         for (i = 0; i < setsize; i++)
       }
    |}]

let%expect_test "give_back_the_comment_characters_for_newline_comments_too" =
  let template =
    {| {:[1]} |}
  in

  let source =
    {|
       {
         // a comment
       }
    |}
  in

  let rewrite_template =
    {|
      {:[1]}
    |}
  in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact
    {|
      {
         // a comment
       }
    |}]
