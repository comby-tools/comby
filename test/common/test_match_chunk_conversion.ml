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
foo(
    bar
    baz
    qux
)
g|} in
  let template = ":[x~\\w+](:[y])" in
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
    "content": "baz(qux)",
    "start": { "offset": 19, "line": 7, "column": 1 },
    "ranges": [
      {
        "start": { "offset": 19, "line": 7, "column": 1 },
        "end": { "offset": 27, "line": 7, "column": 9 }
      }
    ]
  },
  {
    "content": "foo(bar)",
    "start": { "offset": 6, "line": 4, "column": 1 },
    "ranges": [
      {
        "start": { "offset": 6, "line": 4, "column": 1 },
        "end": { "offset": 14, "line": 4, "column": 9 }
      }
    ]
  }
]|}]
