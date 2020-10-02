open Core

open Matchers
open Rewriter

open Matchers.Alpha

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run ?(configuration = configuration) (module M : Matchers.Matcher) source match_template ?rule rewrite_template =
  let open Language in
  let rule =
    match rule with
    | Some rule -> Rule.Alpha.create rule |> Or_error.ok_exn
    | None -> Rule.Alpha.create "where true" |> Or_error.ok_exn
  in
  M.all ~configuration ~template:match_template ~source
  |> List.filter ~f:(fun { Match.environment; _ } -> Rule.Alpha.(sat @@ apply rule environment))
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let%expect_test "regex_holes_simple" =
  let source = {|foo|} in
  let match_template = {|:[x~\w+]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(foo)|}]

let%expect_test "regex_holes_simple_posix" =
  let source = {|foo|} in
  let match_template = {|:[x~[[:alpha:]]]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(f)(o)(o)|}]

let%expect_test "regex_holes_substring" =
  let source = {|foo()|} in
  let match_template = {|:[x~o\w]()|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|f(oo)|}]

let%expect_test "regex_holes_empty_string_terminates" =
  let source = {|foo()|} in
  let match_template = {|:[x~|]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|()f()o()o()(())|}]

let%expect_test "regex_holes_repetition_takes_precedence" =
  let source = {|foobar()|} in
  (* this will _not_ match because bar is consumed by \w before we look ahead *)
  let match_template = {|:[x~\w+]bar()|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]

let%expect_test "regex_holes_negated_match" =
  let source = {|(literally_anyting_except_close_paren?!@#$%^&*[])|} in
  let match_template = {|(:[x~[^)]+])|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(literally_anyting_except_close_paren?!@#$%^&*[])|}];

  let source = {|(arg1, arg2, arg3)|} in
  let match_template = {|:[x~[^,() ]+]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|((arg1), (arg2), (arg3))|}]

let%expect_test "regex_holes_dot_star_ok_and_this_is_for_newline" =
  let source = "foo()\nbar()" in
  let match_template = {|:[x~.*]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(foo())()
(bar())|}]

let%expect_test "regex_holes_optional" =
  let source = "nonovember no november no vember" in
  let match_template = {|:[x~no(vember)?]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(no)(november) (no) (november) (no) vember|}]

let%expect_test "regex_holes_optional_spaces" =
  let source = "nonovember no november no vember" in
  let match_template = {|no :[x~(vember)?]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|nonovember ()november (vember)|}]

(* Note: Known limitation: this behavior does _not_ allow (optional)? to match
   empty string to sat template. Because of something tricky. *)
let%expect_test "regex_holes_optional_doesnt_work_outside_regex" =
  let source = "no" in
  let match_template = {|no:[x~(vember)?]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|No matches.|}];

  let source = "foo bar foobar" in
  let match_template = {|:[x~\s*?]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|()f()o()o() ()b()a()r() ()f()o()o()b()a()r|}];

  let source = "foo bar foobar" in
  let match_template = {|:[x~\s*]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|()f()o()o( )()b()a()r( )()f()o()o()b()a()r|}]


let%expect_test "regex_holes_optional_strip_no_from_november_outside_regex" =
  let source = "nonovember no november no vember" in
  let match_template = {|no:[x~(vember)?]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|()(vember) () (vember) () vember|}]

let%expect_test "regex_holes_optional_strip_no_from_november_inside_regex" =
  let source = "nonovember no november no vember" in
  let match_template = {|:[x~no(vember)?]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(no)(november) (no) (november) (no) vember|}]

let%expect_test "leading_spaces_beginning_line_anchor" =
  let source = {|
       a
   b
            c
|}
  in
  let match_template = {|:[x~^(\s+)]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(       )a(   )b(            )c
|}]

let%expect_test "spaces_star" =
  let source = {|
     a
  b
           c
     d
|}
  in
  let match_template = {|:[x~\s*]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  (* The <spaces><empty space>chars is how this behaves on https://regexr.com/59ft0 as well, see replace *)
  [%expect_exact {|(
     )()a(
  )()b(
           )()c(
     )()d(
)|}]

let%expect_test "end_line_anchor" =
  let source = {|
aaa bbb
aaa bbb ccc
ccc ddd
|}
  in
  let match_template = {|:[x~\w+ bbb$]|} in
  let rewrite_template = {|(:[x])|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|
(aaa bbb)aaa bbb ccc
ccc ddd
|}]

let%expect_test "word_boundaries" =
  let source = {|
foo(bar, baz(),


          qux.derp)
|}
  in
  let match_template = {|:[x~\b\w+\b]|} in
  let rewrite_template = {|(>:[x]<)|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|
(>foo<)((>bar<), (>baz<)(),


          (>qux<).(>derp<))
|}]

(* I previously assumed [^ ] would not match newlines, but it does, and is the
   same as regexr https://regexr.com/59fst. To not match newlines, see the next
   test with [^\s] *)
let%expect_test "do_not_match_space" =
  let source = {|
foo(bar, baz(),


          qux.derp)
|}
  in
  let match_template = {|:[x~[^, ]+]|} in
  let rewrite_template = {|(>:[x]<)|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(>
foo(bar<), (>baz()<),(>


<)          (>qux.derp)
<)|}]

let%expect_test "do_not_match_whitespace" =
  let source = {|
foo(bar, baz(),


          qux.derp)
|}
  in
  let match_template = {|:[x~[^,\s]+]|} in
  let rewrite_template = {|(>:[x]<)|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|
(>foo(bar<), (>baz()<),


          (>qux.derp)<)
|}]

let%expect_test "eof_anchor" =
  let source = {|
setScore(5)
setScore(6)
setScore(6.5)
setScore("")
setScore("hi")
setScore("hi" + "there")
setScore('ho')
setScore(x)
setScore(null)
setScore(4/3.0)
setScore(4.0/3.0)
setScore(4/3)
|}
  in
  let match_template = {|setScore(:[1])|} in
  let rule = {|
     where match :[1] {
     | ":[~^\\d+$]" -> false
     | ":[_]" -> true
     }
  |}
  in
  let rewrite_template = "setScore( /*CHECK ME*/ :[1])" in

  run (module Generic) source match_template ~rule rewrite_template;
  [%expect_exact {|
setScore(5)
setScore(6)
setScore( /*CHECK ME*/ 6.5)
setScore( /*CHECK ME*/ "")
setScore( /*CHECK ME*/ "hi")
setScore( /*CHECK ME*/ "hi" + "there")
setScore( /*CHECK ME*/ 'ho')
setScore( /*CHECK ME*/ x)
setScore( /*CHECK ME*/ null)
setScore( /*CHECK ME*/ 4/3.0)
setScore( /*CHECK ME*/ 4.0/3.0)
setScore( /*CHECK ME*/ 4/3)
|}]
