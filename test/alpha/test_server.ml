open Core

open Lwt.Infix

open Match
open Server_types

let binary_path = "../../../../comby-server"

let port = "9991"

let pid' = ref None

let launch port =
  Unix.create_process ~prog:binary_path ~args:["-p"; port]
  |> fun { pid; _ } -> pid' := Some pid

let post endpoint json =
  let uri =
    let uri endpoint =
      Uri.of_string ("http://127.0.0.1:" ^ port ^ "/" ^ endpoint)
    in
    match endpoint with
    | `Match -> uri "match"
    | `Rewrite -> uri "rewrite"
    | `Substitute -> uri "substitute"
  in
  let thread =
    Cohttp_lwt_unix.Client.post ~body:(`String json) uri >>= fun (_, response) ->
    match response with
    | `Stream response -> Lwt_stream.get response >>= fun result -> Lwt.return result
    | _ -> Lwt.return None
  in
  match Lwt_unix.run thread with
  | None -> "FAIL"
  | Some result -> result

(* FIXME(RVT) use wait *)
let launch () =
  launch port;
  Unix.sleep 2

let kill () =
  match !pid' with
  | None -> ()
  | Some pid ->
    match Signal.send Signal.kill (`Pid pid) with
    | `Ok -> ()
    | `No_such_process -> ()

let () = launch ()

let%expect_test "post_request" =

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] == "world"|} in
  let language = "generic" in

  In.{ source; match_template; rule; language; id = 0 }
  |> In.match_request_to_yojson
  |> Yojson.Safe.to_string
  |> post `Match
  |> print_string;

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
      "source": "hello world",
      "id": 0
    } |}];


  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] = "world"|} in
  let language = "generic" in

  In.{ source; match_template; rule; language; id = 0 }
  |> In.match_request_to_yojson
  |> Yojson.Safe.to_string
  |> post `Match
  |> print_string;

  [%expect {|
    Error in line 1, column 7:
    where :[1] = "world"
          ^
    Expecting "false", "match", "rewrite" or "true"
    Backtracking occurred after:
      Error in line 1, column 12:
      where :[1] = "world"
                 ^
      Expecting "!=" or "==" |}];

  let substitution_kind = "in_place" in
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where :[1] == "world"|} in
  let rewrite_template = ":[1], hello" in
  let language = "generic" in

  In.{ source; match_template; rewrite_template; rule; language; substitution_kind; id = 0}
  |> In.rewrite_request_to_yojson
  |> Yojson.Safe.to_string
  |> post `Rewrite
  |> print_string;

  [%expect {|
      {
        "rewritten_source": "world, hello",
        "in_place_substitutions": [
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
        ],
        "id": 0
      } |}];

  let substitution_kind = "newline_separated" in
  let source = "hello world {} hello world" in
  let match_template = "hello :[[1]]" in
  let rule = Some {|where :[1] == "world"|} in
  let rewrite_template = ":[1], hello" in
  let language = "generic" in

  In.{ source; match_template; rewrite_template; rule; language; substitution_kind; id = 0}
  |> In.rewrite_request_to_yojson
  |> Yojson.Safe.to_string
  |> post `Rewrite
  |> print_string;

  [%expect {|
      {
        "rewritten_source": "world, hello\nworld, hello",
        "in_place_substitutions": [],
        "id": 0
      } |}];

  (* test there must be at least one predicate in a rule *)
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rule = Some {|where |} in
  let language = "generic" in

  let request = In.{ source; match_template; rule; language; id = 0 } in
  let json = In.match_request_to_yojson request |> Yojson.Safe.to_string in
  let result = post `Match json in

  print_string result;
  [%expect {|
    Error in line 1, column 7:
    where
          ^
    Expecting ":[", "false", "match", "rewrite", "true" or string literal |}]

let%expect_test "post_substitute" =

  let rewrite_template = ":[1] hi :[2]" in
  let environment = Environment.create () in
  let environment = Environment.add environment "1" "oh" in
  let environment = Environment.add environment "2" "there" in

  In.{ rewrite_template; environment; id = 0 }
  |> In.substitution_request_to_yojson
  |> Yojson.Safe.to_string
  |> post `Substitute
  |> print_string;

  [%expect {| { "result": "oh hi there", "id": 0 } |}]

let () = kill ()
