open Core

open Test_helpers
open Comby_kernel

open Matchers

let%expect_test "match_chunk_conversion" =
  let source = {|a
b
c
foo(bar)
d
e
baz(qux)
f
g|} in
  let template = ":[x](:[y])" in
  let matches =
    Alpha.Generic.all ~configuration ~template ~source ()
    |> List.map ~f:(Match.convert_offset ~fast:true ~source)
    |> List.map ~f:(fun Match.{ range; _ } -> range)
    |> Match.to_chunks source
    |> (fun m -> `List (List.map m ~f:(Match.chunk_match_to_yojson)))
    |> Yojson.Safe.pretty_to_string
  in
  print_string matches;
  [%expect_exact {|[
  {
    "content": "a\nb\nc\nfoo(bar)\nd\ne\nbaz(qux)",
    "start": { "offset": 0, "line": 1, "column": 0 },
    "ranges": [
      {
        "start": { "offset": 0, "line": 1, "column": 1 },
        "end": { "offset": 14, "line": 4, "column": 9 }
      },
      {
        "start": { "offset": 14, "line": 4, "column": 9 },
        "end": { "offset": 27, "line": 7, "column": 9 }
      }
    ]
  }
]|}]
