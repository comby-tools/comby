open Core
open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "rewrite_comments_1" =
  let template = "replace this :[1] end" in
  let source = "/* don't replace this () end */ do replace this () end" in
  let rewrite_template = "X" in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact "/* don't replace this () end */ do X"];
  run (module Omega.C) source template rewrite_template;
  [%expect_exact "/* don't replace this () end */ do X"]

let%expect_test "rewrite_comments_2" =
  let template = {|
      if (:[1]) { :[2] }
    |} in
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
  let rewrite_template = {|
      if (:[1]) {}
    |} in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact {|
      if (real_condition_body_must_be_empty) {}
    |}];
  run (module Omega.C) source template rewrite_template;
  [%expect_exact {|
      if (real_condition_body_must_be_empty) {}
    |}]

let%expect_test "capture_comments" =
  let template = {|if (:[1]) { :[2] }|} in
  let source = {|if (true) { /* some comment */ console.log(z); }|} in
  run_all_matches (module Alpha.C) source template;
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":48,"line":1,"column":49}},"environment":[{"variable":"1","value":"true","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":8,"line":1,"column":9}}},{"variable":"2","value":"console.log(z);","range":{"start":{"offset":31,"line":1,"column":32},"end":{"offset":46,"line":1,"column":47}}}],"matched":"if (true) { /* some comment */ console.log(z); }"}]}
|}];
  run_all_matches (module Omega.C) source template;
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":48,"line":1,"column":49}},"environment":[{"variable":"1","value":"true","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":8,"line":1,"column":9}}},{"variable":"2","value":"console.log(z);","range":{"start":{"offset":31,"line":1,"column":32},"end":{"offset":46,"line":1,"column":47}}}],"matched":"if (true) { /* some comment */ console.log(z); }"}]}
|}]

let%expect_test "single_quote_in_comment" =
  let template = {| {:[1]} |} in
  let source = {|
       /*'*/
       {test}
    |} in
  let rewrite_template = {|
      {:[1]}
    |} in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact {|
      {test}
    |}];
  run (module Omega.C) source template rewrite_template;
  [%expect_exact {|
      {test}
    |}]

let%expect_test "single_quote_in_comment" =
  let template = {| {:[1]} |} in
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
  let rewrite_template = {|
      {:[1]}
    |} in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact
    {|
      {
         a = 1;
         /* Events with mask == AE_NONE are not set. So let's initiaize the
          * vector with it. */
         for (i = 0; i < setsize; i++)
       }
    |}];
  run (module Omega.C) source template rewrite_template;
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
  let template = {| {:[1]} |} in
  let source =
    {|
       {
         a = 1;
         /* ' */
         for (i = 0; i < setsize; i++)
       }
    |}
  in
  let rewrite_template = {|
      {:[1]}
    |} in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact
    {|
      {
         a = 1;
         /* ' */
         for (i = 0; i < setsize; i++)
       }
    |}];
  run (module Omega.C) source template rewrite_template;
  [%expect_exact
    {|
      {
         a = 1;
         /* ' */
         for (i = 0; i < setsize; i++)
       }
    |}]

let%expect_test "give_back_the_comment_characters_for_newline_comments_too" =
  let template = {| {:[1]} |} in
  let source = {|
       {
         // a comment
       }
    |} in
  let rewrite_template = {|
      {:[1]}
    |} in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact {|
      {
         // a comment
       }
    |}];
  run (module Omega.C) source template rewrite_template;
  [%expect_exact {|
      {
         // a comment
       }
    |}]

let%expect_test "comments_in_templates_imply_whitespace" =
  let template = {|
/* f */
// q
a
|} in
  let source = {|
// idgaf
/* fooo */
a
|} in
  let rewrite_template = {|erased|} in
  run (module Alpha.C) source template rewrite_template;
  [%expect_exact {|erased|}];
  run (module Omega.C) source template rewrite_template;
  [%expect_exact {|erased|}]
