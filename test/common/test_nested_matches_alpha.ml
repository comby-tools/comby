open Core

open Test_helpers

include Test_alpha

let run ?(m = (module Generic : Matchers.Matcher)) ?(configuration = configuration) source match_template () =
  let (module M) = m in
  M.all ~configuration ~nested:true ~template:match_template ~source ()
  |> function
  | [] -> print_string "No matches."
  | matches ->
    let matches = List.map matches ~f:(Match.convert_offset ~fast:true ~source) in
    Format.asprintf "%a" Match.pp (None, matches)
    |> print_string

let%expect_test "nested_matches" =
  let source = {|
a{
   b{
      c{
        d{e}
       }
    }
 }

q{
 b{
  f{}
 }
}
|} in
  let match_template = {|:[[f]]{:[x]}|} in
  run source match_template ();
  [%expect_exact {|2:a{\n   b{\n      c{\n        d{e}\n       }\n    }\n }
3:b{\n      c{\n        d{e}\n       }\n    }
4:c{\n        d{e}\n       }
5:d{e}
10:q{\n b{\n  f{}\n }\n}
11:b{\n  f{}\n }
12:f{}
|}]

let%expect_test "nested_matches" =
  let source = {|a(b(c(d(e))))|} in
  let match_template = {|:[[f]](:[x])|} in
  run source match_template ();
  [%expect_exact {|1:a(b(c(d(e))))
1:b(c(d(e)))
1:c(d(e))
1:d(e)
|}]
