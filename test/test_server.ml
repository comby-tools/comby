open Core

open Lwt.Infix

type match_request =
  { source : string
  ; match_template : string [@key "match"]
  ; rule : string option [@default None]
  ; language : string [@default "generic"]
  }
[@@deriving yojson]

type rewrite_request =
  { source : string
  ; match_template : string [@key "match"]
  ; rewrite_template : string [@key "rewrite"]
  ; rule : string option [@default None]
  ; language : string [@default "generic"]
  ; substitution_kind : string [@default "in_place"]
  }
[@@deriving yojson]

let binary_path = "../../../comby-server"

let launch () =
  Unix.create_process ~prog:binary_path ~args:["-p"; "9898"]
  |> fun { pid; _ } -> pid

let post uri json =
  let thread =
    Cohttp_lwt_unix.Client.post ~body:(`String json) uri >>= fun (_, response) ->
    match response with
    | `Stream response -> Lwt_stream.get response >>= fun result -> Lwt.return result
    | _ -> Lwt.return None
  in

  match Lwt_unix.run thread with
  | None -> "FAIL"
  | Some result -> result


let%expect_test "post_match_request" =
  let pid = launch () in
  let uri = Uri.of_string "http://127.0.0.1:9898/match" in

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] == "world"|} in
  let language = "generic" in

  let request = { source; match_template; rule; language } in
  let json = match_request_to_yojson request |> Yojson.Safe.to_string in

  let result = post uri json in

  match Signal.send Signal.kill (`Pid pid) with
  | `Ok ->
    print_string json;
    [%expect "{\"source\":\"hello world\",\"match\":\"hello :[1]\",\"rule\":\"where :[1] == \\\"world\\\"\"}"];
    print_string result;
    [%expect {|
      {
        "matches": [
          {
            "range": {
              "start": { "offset": 0, "line": 1, "column": 1 },
              "end": { "offset": 11, "line": 1, "column": 12 }
            },
            "environment": [
              {
                "variable": "1",
                "value": "world",
                "range": {
                  "start": { "offset": 6, "line": 1, "column": 7 },
                  "end": { "offset": 11, "line": 1, "column": 12 }
                }
              }
            ],
            "matched": "hello world"
          }
        ],
        "source": "hello world"
      } |}]
  | `No_such_process ->
    [%expect {| DID NOT REACH THIS PROGRAM POINT |}]

let%expect_test "post_match_request_malformed_rule" =
  let pid = launch () in
  let uri = Uri.of_string "http://127.0.0.1:9898/match" in

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] = "world"|} in
  let language = "generic" in

  let request = { source; match_template; rule; language } in
  let json = match_request_to_yojson request |> Yojson.Safe.to_string in

  let result = post uri json in

  match Signal.send Signal.kill (`Pid pid) with
  | `Ok -> print_string result;
    [%expect {|
      Error in line 1, column 7:
      where :[1] = "world"
            ^
      Expecting "false", "match", "rewrite", "true", end of input or string literal
      Backtracking occurred after:
        Error in line 1, column 12:
        where :[1] = "world"
                   ^
        Expecting "!=" or "==" |}]
  | `No_such_process ->
    [%expect {| DID NOT REACH THIS PROGRAM POINT |}]


let%expect_test "post_rewrite_reqeust_in_place" =
  let pid = launch () in
  let uri = Uri.of_string "http://127.0.0.1:9898/rewrite" in
  let substitution_kind = "in_place" in

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] == "world"|} in
  let rewrite_template = ":[1], hello" in
  let language = "generic" in

  let request = { source; match_template; rewrite_template; rule; language; substitution_kind} in
  let json = rewrite_request_to_yojson request |> Yojson.Safe.to_string in

  let result = post uri json in

  match Signal.send Signal.kill (`Pid pid) with
  | `Ok -> print_string result;
    [%expect {|
      {
        "rewritten_source": "world, hello",
        "contextual_substitutions": [
          {
            "range": {
              "start": { "offset": 0, "line": -1, "column": -1 },
              "end": { "offset": 12, "line": -1, "column": -1 }
            },
            "replacement_content": "world, hello",
            "environment": [
              {
                "variable": "1",
                "value": "world",
                "range": {
                  "start": { "offset": 0, "line": -1, "column": -1 },
                  "end": { "offset": 5, "line": -1, "column": -1 }
                }
              }
            ]
          }
        ]
      } |}]
  | `No_such_process ->
    [%expect {| DID NOT REACH THIS PROGRAM POINT |}]

let%expect_test "post_rewrite_reqeust_newline" =
  let pid = launch () in
  let uri = Uri.of_string "http://127.0.0.1:9898/rewrite" in
  let substitution_kind = "newline" in

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] == "world"|} in
  let rewrite_template = ":[1], hello" in
  let language = "generic" in

  let request = { source; match_template; rewrite_template; rule; language; substitution_kind} in
  let json = rewrite_request_to_yojson request |> Yojson.Safe.to_string in

  let result = post uri json in

  match Signal.send Signal.kill (`Pid pid) with
  | `Ok -> print_string result;
    [%expect {|
      { "rewritten_source": "world, hello", "contextual_substitutions": [] } |}]
  | `No_such_process ->
    [%expect {| DID NOT REACH THIS PROGRAM POINT |}]
