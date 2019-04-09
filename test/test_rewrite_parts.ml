open Core

open Match
open Matchers
open Rewriter

let%expect_test "get_offsets_for_holes" =
  let rewrite_template = {|1234:[1]1234:[2]|} in
  let result = Rewrite_template.get_offsets_for_holes rewrite_template ["1"; "2"] in
  print_s [%message (result : (string * int) list)];
  [%expect_exact {|(result ((2 8) (1 4)))
|}]

let%expect_test "get_offsets_for_holes_after_substitution_1" =
  let rewrite_template = {|1234:[1]1234:[2]|} in
  let offsets = Rewrite_template.get_offsets_for_holes rewrite_template ["1"; "2"] in
  let environment =
    Environment.create ()
    |> (fun environment -> Environment.add environment "1" "333")
    |> (fun environment -> Environment.add environment "2" "22")
  in
  let result = Rewrite_template.get_offsets_after_substitution offsets environment in
  print_s [%message (result : (string * int) list)];
  [%expect_exact {|(result ((2 11) (1 4)))
|}]

let%expect_test "get_offsets_for_holes_after_substitution_1" =
  let rewrite_template = {|1234:[1]1234:[3]11:[2]|} in
  let offsets = Rewrite_template.get_offsets_for_holes rewrite_template ["1"; "3"; "2"] in
  let environment =
    Environment.create ()
    |> (fun environment -> Environment.add environment "1" "333")
    |> (fun environment -> Environment.add environment "3" "333")
    |> (fun environment -> Environment.add environment "2" "22")
  in
  let result = Rewrite_template.get_offsets_after_substitution offsets environment in
  print_s [%message (result : (string * int) list)];
  [%expect_exact {|(result ((2 16) (3 11) (1 4)))
|}]


let configuration = Configuration.create ~match_kind:Fuzzy ()

let all ?(configuration = configuration) template source =
  C.all ~configuration ~template ~source

let%expect_test "comments_in_string_literals_should_not_be_treated_as_comments_by_fuzzy" =
  let source = {|123433312343331122|} in
  let match_template = {|1234:[1]1234:[3]11:[2]|} in
  let rewrite_template = {|1234:[1]1234:[3]11:[2]|} in
  all match_template source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some rewrite_result -> print_string (Yojson.Safe.pretty_to_string (Rewrite.result_to_yojson rewrite_result))
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|{
  "rewritten_source": "123433312343331122",
  "contextual_substitutions": [
    {
      "range": {
        "start": { "offset": 0, "line": -1, "column": -1 },
        "end": { "offset": 18, "line": -1, "column": -1 }
      },
      "replacement_content": "123433312343331122",
      "environment": [
        {
          "variable": "1",
          "value": "333",
          "range": {
            "start": { "offset": 4, "line": -1, "column": -1 },
            "end": { "offset": 7, "line": -1, "column": -1 }
          }
        },
        {
          "variable": "2",
          "value": "22",
          "range": {
            "start": { "offset": 16, "line": -1, "column": -1 },
            "end": { "offset": 18, "line": -1, "column": -1 }
          }
        },
        {
          "variable": "3",
          "value": "333",
          "range": {
            "start": { "offset": 11, "line": -1, "column": -1 },
            "end": { "offset": 14, "line": -1, "column": -1 }
          }
        }
      ]
    }
  ]
}|}]

let%expect_test "comments_in_string_literals_should_not_be_treated_as_comments_by_fuzzy" =
  let source = {|123433312343331122;123433312343331122;|} in
  let match_template = {|1234:[1]1234:[3]11:[2];|} in
  let rewrite_template = {|1234:[1]1234:[3]11:[2];|} in
  all match_template source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some rewrite_result -> print_string (Yojson.Safe.pretty_to_string (Rewrite.result_to_yojson rewrite_result))
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|{
  "rewritten_source": "123433312343331122;123433312343331122;",
  "contextual_substitutions": [
    {
      "range": {
        "start": { "offset": 19, "line": -1, "column": -1 },
        "end": { "offset": 38, "line": -1, "column": -1 }
      },
      "replacement_content": "123433312343331122;",
      "environment": [
        {
          "variable": "1",
          "value": "333",
          "range": {
            "start": { "offset": 4, "line": -1, "column": -1 },
            "end": { "offset": 7, "line": -1, "column": -1 }
          }
        },
        {
          "variable": "2",
          "value": "22",
          "range": {
            "start": { "offset": 16, "line": -1, "column": -1 },
            "end": { "offset": 18, "line": -1, "column": -1 }
          }
        },
        {
          "variable": "3",
          "value": "333",
          "range": {
            "start": { "offset": 11, "line": -1, "column": -1 },
            "end": { "offset": 14, "line": -1, "column": -1 }
          }
        }
      ]
    },
    {
      "range": {
        "start": { "offset": 0, "line": -1, "column": -1 },
        "end": { "offset": 19, "line": -1, "column": -1 }
      },
      "replacement_content": "123433312343331122;",
      "environment": [
        {
          "variable": "1",
          "value": "333",
          "range": {
            "start": { "offset": 4, "line": -1, "column": -1 },
            "end": { "offset": 7, "line": -1, "column": -1 }
          }
        },
        {
          "variable": "2",
          "value": "22",
          "range": {
            "start": { "offset": 16, "line": -1, "column": -1 },
            "end": { "offset": 18, "line": -1, "column": -1 }
          }
        },
        {
          "variable": "3",
          "value": "333",
          "range": {
            "start": { "offset": 11, "line": -1, "column": -1 },
            "end": { "offset": 14, "line": -1, "column": -1 }
          }
        }
      ]
    }
  ]
}|}]

let%expect_test "multiple_contextual_substitutions" =
  let source = {|foo bar foo|} in
  let match_template = {|foo|} in
  let rewrite_template = {|xxxx|} in
  all match_template source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some rewrite_result -> print_string (Yojson.Safe.pretty_to_string (Rewrite.result_to_yojson rewrite_result))
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|{
  "rewritten_source": "xxxx bar xxxx",
  "contextual_substitutions": [
    {
      "range": {
        "start": { "offset": 9, "line": -1, "column": -1 },
        "end": { "offset": 13, "line": -1, "column": -1 }
      },
      "replacement_content": "xxxx",
      "environment": []
    },
    {
      "range": {
        "start": { "offset": 0, "line": -1, "column": -1 },
        "end": { "offset": 4, "line": -1, "column": -1 }
      },
      "replacement_content": "xxxx",
      "environment": []
    }
  ]
}|}]
