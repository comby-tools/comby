open Core

open Match

open Test_helpers

include Test_alpha

let%expect_test "rule_sat" =
  let rule = {| where "x" != "y" |} in
  sat rule |> print_string;
  [%expect_exact {|true|}];

  let rule = {| where "x" != "x" |} in
  sat rule |> print_string;
  [%expect_exact {|false|}];

  let rule = {| where "x" == "x" |} in
  sat rule |> print_string;
  [%expect_exact {|true|}];

  let rule = {| where "x" == "y" |} in
  sat rule |> print_string;
  [%expect_exact {|false|}];

  let rule = {| where :[x] == "y" |} in
  sat rule |> print_string;
  [%expect_exact {|false|}];

  let rule = {| where :[x] == :[x] |} in
  sat rule |> print_string;
  [%expect_exact {|false|}]

let%expect_test "rule_sat_with_env" =
  let env = make_env ["1", "x"; "2", "y"; "3", "x"] in

  let rule = {| where :[1] == :[3], :[1] != :[2] |} in
  sat ~env rule |> print_string;
  [%expect_exact {|true|}];

  let rule = {| where :[1] == :[3], :[1] != "y" |} in
  sat ~env rule |> print_string;
  [%expect_exact {|true|}];

  let rule = {| where :[1] == :[3], :[1] == "x" |} in
  sat ~env rule |> print_string;
  [%expect_exact {|true|}];

  let rule = {| where :[1] == :[2], :[1] != :[2] |} in
  sat ~env rule |> print_string;
  [%expect_exact {|false|}]

let%expect_test "where_true" =
  let template =
    {|
      (:[1]) => {}
    |}
    |> format
  in

  let source =
    {|
      (b,c) => {}
    |}
    |> format
  in

  let rule =
    {| where true
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 11, "line": 1, "column": 12 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "b,c",
            "range": {
              "start": { "offset": 1, "line": 1, "column": 2 },
              "end": { "offset": 4, "line": 1, "column": 5 }
            }
          }
        ],
        "matched": "(b,c) => {}"
      }
    ] |}]

let%expect_test "match_sat" =
  let template =
    {|
      (:[1]) => {}
    |}
    |> format
  in

  let source =
    {|
      (b,c) => {}
    |}
    |> format
  in

  let rule =
    {| where
       match :[1] {
       | ":[_],:[_]" -> false
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}];

  let rule =
    {| where
       match :[1] {
       | ":[_],:[_]" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 11, "line": 1, "column": 12 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "b,c",
            "range": {
              "start": { "offset": 1, "line": 1, "column": 2 },
              "end": { "offset": 4, "line": 1, "column": 5 }
            }
          }
        ],
        "matched": "(b,c) => {}"
      }
    ] |}];

  let source =
    {|
      (a) => {}
      (b,c) => {}
    |}
    |> format
  in

  let rule =
    {| where
       match :[1] {
       | ":[_],:[_]" -> false
       | ":[_]" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 9, "line": 1, "column": 10 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "a",
            "range": {
              "start": { "offset": 1, "line": 1, "column": 2 },
              "end": { "offset": 2, "line": 1, "column": 3 }
            }
          }
        ],
        "matched": "(a) => {}"
      }
    ] |}];

  let rule =
    {|
       where
       match :[1] {
       | ":[_],:[_]" -> false
       | ":[_]" -> :[1] == "a"
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 9, "line": 1, "column": 10 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "a",
            "range": {
              "start": { "offset": 1, "line": 1, "column": 2 },
              "end": { "offset": 2, "line": 1, "column": 3 }
            }
          }
        ],
        "matched": "(a) => {}"
      }
    ] |}];

  let rule =
    {| where
       match :[1] {
       | ":[_],:[_]" -> false
       | ":[_]" -> :[1] == "b"
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}]


let%expect_test "match_s_suffix" =
  let template = ":[1]s" in

  let source = "names" in

  let rule =
    {| where true
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 5, "line": 1, "column": 6 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "name",
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 4, "line": 1, "column": 5 }
            }
          }
        ],
        "matched": "names"
      }
    ] |}]

let%expect_test "match_s_suffix" =
  let template = ":[1]" in

  let source = "names" in

  let rule =
    {| where true
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 5, "line": 1, "column": 6 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "names",
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 5, "line": 1, "column": 6 }
            }
          }
        ],
        "matched": "names"
      }
    ] |}]

let%expect_test "configuration_choice_based_on_case" =
  let template = ":[1]" in

  let source = "names" in

  let rule =
    {| where match :[1] {
       | "ame" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}];

  let template = ":[1]" in

  let source = "names" in

  let rule =
    {| where match :[1] {
       | "names" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 5, "line": 1, "column": 6 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "names",
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 5, "line": 1, "column": 6 }
            }
          }
        ],
        "matched": "names"
      }
    ] |}];

  let template = ":[1]" in

  let source = "namesXXXXX" in

  let rule =
    {| where match :[1] {
       | "names" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}]




let%expect_test "match_using_environment_merge" =
  let template = "{:[1]}" in

  let source = "{{ a : a } { a : a }}" in

  let rule =
    {| where match :[1] { | "{ :[x] : :[y] }" -> :[x] == :[y] }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 21, "line": 1, "column": 22 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "{ a : a } { a : a }",
            "range": {
              "start": { "offset": 1, "line": 1, "column": 2 },
              "end": { "offset": 20, "line": 1, "column": 21 }
            }
          }
        ],
        "matched": "{{ a : a } { a : a }}"
      }
    ] |}];

  let template = "{:[1]}" in

  let source = "{{ a : a } { a : b }}" in

  let rule =
    {| where match :[1] { | "{ :[x] : :[y] }" -> :[x] == :[y] }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}]


let%expect_test "nested_matches" =
  let template = ":[1]" in

  let source = "{ { foo : { bar : { baz : qux } } } }" in

  let rule =
    {| where match :[1] {
       | "{ :[foo] : :[tail1] }" -> match :[tail1] {
                     | "{ :[bar] : :[tail2] }" -> match :[tail2] {
                                                 | "{ baz : :[qux] }" -> :[qux] == "qux", :[bar] == "bar"
                                                 }
                     }
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 37, "line": 1, "column": 38 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "{ { foo : { bar : { baz : qux } } } }",
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 37, "line": 1, "column": 38 }
            }
          }
        ],
        "matched": "{ { foo : { bar : { baz : qux } } } }"
      }
    ] |}];

  let template = ":[1]" in

  let source = "{ { foo : { bar : { baz : qux } } } }" in

  let rule =
    {| where match :[1] {
       | "{ :[foo] : :[tail1] }" -> match :[tail1] {
                     | "{ :[bar] : :[tail2] }" -> match :[tail2] {
                                                 | "{ baz : :[qux] }" -> :[qux] == "fail"
                                                 }
                     }
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}]


let%expect_test "match_on_template" =
  let template = ":[1]" in

  let source = "oodles" in

  let rule =
    {| where match "p:[1]" {
       | "poodles" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 6, "line": 1, "column": 7 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "oodles",
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 6, "line": 1, "column": 7 }
            }
          }
        ],
        "matched": "oodles"
      }
    ] |}];

  let template = ":[1]" in

  let source = "poodle" in

  let rule =
    {| where match ":[1]s" {
       | "poodles" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [
      {
        "range": {
          "start": { "offset": 0, "line": 1, "column": 1 },
          "end": { "offset": 6, "line": 1, "column": 7 }
        },
        "environment": [
          {
            "variable": "1",
            "value": "poodle",
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 6, "line": 1, "column": 7 }
            }
          }
        ],
        "matched": "poodle"
      }
    ] |}];

  let template = ":[1]" in

  let source = "poodle" in

  let rule =
    {| where match ":[1]," {
       | "poodle" -> true
       }
    |}
    |> Language.Rule.create
    |> Or_error.ok_exn
  in

  Generic.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } -> Rule.(sat @@ apply rule environment))
  |> print_matches;
  [%expect {|
    [] |}];
