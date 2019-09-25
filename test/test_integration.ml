open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let all ?(configuration = configuration) template source =
  Generic.all ~configuration ~template ~source

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let%expect_test "dont_get_stuck" =
  let template = "" in
  let source = "a" in
  let matches = all ~configuration template source in
  print_matches matches;
  [%expect_exact {|[]|}]

let%expect_test "dont_get_stuck" =
  let template = "a" in
  let source = "a" in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": 1, "column": 1 },
      "end": { "offset": 1, "line": 1, "column": 2 }
    },
    "environment": [],
    "matched": "a"
  }
]|}]

let%expect_test "dont_get_stuck" =
  let template = "a" in
  let source = "aaa" in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": 1, "column": 1 },
      "end": { "offset": 1, "line": 1, "column": 2 }
    },
    "environment": [],
    "matched": "a"
  },
  {
    "range": {
      "start": { "offset": 1, "line": 1, "column": 2 },
      "end": { "offset": 2, "line": 1, "column": 3 }
    },
    "environment": [],
    "matched": "a"
  },
  {
    "range": {
      "start": { "offset": 2, "line": 1, "column": 3 },
      "end": { "offset": 3, "line": 1, "column": 4 }
    },
    "environment": [],
    "matched": "a"
  }
]|}]

let%expect_test "rewrite_awesome_1" =
  let template = "replace this :[1] end" in
  let source = "xreplace this () end" in
  let rewrite_template = "X" in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "xX"]

let%expect_test "rewrite_whole_template_matches" =
  let template = {|rewrite :[1] <- this string|} in
  let source = {|rewrite hello world <- this string|} in
  let rewrite_template = "?" in

  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "?"]

let%expect_test "single_token" =
  let template = {|:[[1]] this|} in
  let source = {|the problem is this|} in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 12, "line": 1, "column": 13 },
      "end": { "offset": 19, "line": 1, "column": 20 }
    },
    "environment": [
      {
        "variable": "1",
        "value": "is",
        "range": {
          "start": { "offset": 12, "line": 1, "column": 13 },
          "end": { "offset": 14, "line": 1, "column": 15 }
        }
      }
    ],
    "matched": "is this"
  }
]|}]


let%expect_test "single_token_with_preceding_whitespace" =
  let template = {| :[[1]] this|} in
  let source = {|the problem is this|} in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 11, "line": 1, "column": 12 },
      "end": { "offset": 19, "line": 1, "column": 20 }
    },
    "environment": [
      {
        "variable": "1",
        "value": "is",
        "range": {
          "start": { "offset": 12, "line": 1, "column": 13 },
          "end": { "offset": 14, "line": 1, "column": 15 }
        }
      }
    ],
    "matched": " is this"
  }
]|}]

let%expect_test "single_token_rewrite" =
  let template = {| :[[1]] this|} in
  let source = {|the problem is this|} in
  let rewrite_template = ":[1]" in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "the problemis"]

let%expect_test "single_token_match_inside_paren_no_succeeding_whitespace" =
  let template = {|:[[1]](:[[2]])|} in
  let source = {|foo(bar)|} in
  let rewrite_template = ":[1] : :[2]" in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "foo : bar"]

let%expect_test "whitespace_hole_rewrite" =
  let template = {|:[ w]this|} in
  let rewrite_template = "space:[ w]here" in
  let source = {|      this|} in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "space      here"]

let%expect_test "punctuation_hole_rewrite" =
  let template = {|:[x.]|} in
  let rewrite_template = "->:[x.]<-" in
  let source = {|now.this. is,pod|racing|} in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "->now.this.<- ->is,pod|racing<-"]

let%expect_test "newline_hole_rewrite" =
  let template = {|:[x\n]|} in
  let rewrite_template = {|->:[x\n]<-|} in
  let source = {|now.this.
is,pod|racing
|} in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact "->now.this.
<-->is,pod|racing
<-"]



let%expect_test "shift_or_at_least_dont_get_stuck" =
  let template = ":[1]" in
  let source = "a" in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": 1, "column": 1 },
      "end": { "offset": 1, "line": 1, "column": 2 }
    },
    "environment": [
      {
        "variable": "1",
        "value": "a",
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 1, "line": 1, "column": 2 }
        }
      }
    ],
    "matched": "a"
  }
]|}]

let%expect_test "shift_or_at_least_dont_get_stuck" =
  let template = ":[1]" in
  let source = "aa" in
  let matches = all template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": 1, "column": 1 },
      "end": { "offset": 2, "line": 1, "column": 3 }
    },
    "environment": [
      {
        "variable": "1",
        "value": "aa",
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 2, "line": 1, "column": 3 }
        }
      }
    ],
    "matched": "aa"
  }
]|}]

let%expect_test "nested_rewrite1" =
  let source =
    {|
      x x y strcpy(strcpy(dst1, src1), src2); blah blah XXX
    |}
  in

  let template =
    {|
      strcpy(:[1], :[2])
    |}
  in

  let matches = all template source in
  print_matches matches;
  [%expect_exact "[]"]

(* FIXME(RVT) nested rewrites *)
let%expect_test "nested_rewrite2" =
  let template =
    {|
    if :[var_check] != nil {
      for :[defines] := range :[var_use] {:[inner_body]}
    }
    |}
  in
  let source =
    {|
    if fields.List != nil {
      for _, field := range fields.List {
        if field.Names != nil {
          for _, fieldName := range field.Names {
            stuff with fields and things
          }
        }
      }
    }
    |}
  in
  let rewrite_template = "for :[defines] := range :[var_use] {:[inner_body]}" in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|for _, field := range fields.List {
        if field.Names != nil {
          for _, fieldName := range field.Names {
            stuff with fields and things
          }
        }
      }|}]

let%expect_test "match_:[[1]]" =
  let template =
    {|
    :[[1]].next()
    |}
  in
  let source =
    {|
    col_names = reader.next()
    }
    |}
  in
  let rewrite_template = "next(:[1])" in
  all template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|
    col_names =next(reader)}
    |}]
