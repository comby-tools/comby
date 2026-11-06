open Core
open Comby_kernel
open Test_helpers
open Matchers

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

let%expect_test "regex_holes_optional_doesnt_work_outside_regex" =
  let source = "no" in
  let match_template = {|no:[x~(vember)?]|} in
  let rewrite_template = {|(:[x])|} in
  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|()|}];
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
|} in
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
|} in
  let match_template = {|:[x~\s*]|} in
  let rewrite_template = {|(:[x])|} in
  run (module Generic) source match_template rewrite_template;
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
|} in
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
|} in
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
|} in
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
|} in
  let match_template = {|:[x~[^,\s]+]|} in
  let rewrite_template = {|(>:[x]<)|} in
  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|
(>foo(bar<), (>baz()<),


          (>qux.derp)<)
|}]

let%expect_test "eof_anchor" =
  let source =
    {|
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
  let rule =
    {|
     where match :[1] {
     | ":[~^\\d+$]" -> false
     | ":[_]" -> true
     }
  |}
  in
  let rewrite_template = "setScore( /*CHECK ME*/ :[1])" in
  run (module Generic) source match_template ~rule rewrite_template;
  [%expect_exact
    {|
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

(* Prints each match in the source, as <match>. *)
let show ~source pattern =
  let match_template = Format.sprintf ":[x~%s]" pattern in
  print_string (Format.sprintf "%-16s %S -> " pattern source);
  run (module Generic) source match_template "<:[x]>";
  print_endline ""

let%expect_test "regex_hole_extent_with_classes_and_escapes" =
  show ~source:"abc" {|b[^a]|};
  show ~source:"xa]y" {|a\]|};
  show ~source:"a]b" {|[a\]]+|};
  show ~source:"]a b" {|[]a]+|};
  show ~source:"a[b]" {|[^[]+|};
  show ~source:"a b" {|[^[:space:]]+|};
  show ~source:"a:b" {|a[:]b|};
  [%expect
    {|
    b[^a]            "abc" -> a<bc>
    a\]              "xa]y" -> x<a]>y
    [a\]]+           "a]b" -> <a]>b
    []a]+            "]a b" -> <]a> b
    [^[]+            "a[b]" -> <a>[<b]>
    [^[:space:]]+    "a b" -> <a> <b>
    a[:]b            "a:b" -> <a:b>
    |}]

let%expect_test "regex_syntax_classes_do_not_split_utf8" =
  show ~source:"café x" {|\w+|};
  show ~source:"café x" {|\W+|};
  show ~source:"café" {|[[:alpha:]]+|};
  show ~source:"café x" {|\S+|};
  [%expect
    {|
    \w+              "caf\195\169 x" -> <caf>é <x>
    \W+              "caf\195\169 x" -> caf<é >x
    [[:alpha:]]+     "caf\195\169" -> <caf>é
    \S+              "caf\195\169 x" -> <café> <x>
    |}]

let%expect_test "regex_syntax_negated_shorthand_in_class" =
  (* Re.Perl parses [\W] as [\w]. *)
  show ~source:"a-b" {|[\W]+|};
  show ~source:"a-b" {|[\D]+|};
  [%expect
    {|
    [\W]+            "a-b" -> a<->b
    [\D]+            "a-b" -> <a-b>
    |}]

let%expect_test "regex_syntax_inline_flags" =
  show ~source:"FOO" {|(?i)foo|};
  show ~source:"fOO" {|f(?i)oo|};
  show ~source:"FOO" {|(?i:f)oo|};
  show ~source:"AB" {|(?i)a(?-i)b|};
  show ~source:"x C" {|(a(?i)b|c)|};
  show ~source:"[^A]a" {|(?i)[^a]|};
  show ~source:"a\nb" {|(?s)a.b|};
  show ~source:"ab" {|(?x) a  b # comment|};
  show ~source:"aa" {|(?x) a +|};
  [%expect
    {|
    (?i)foo          "FOO" -> <FOO>
    f(?i)oo          "fOO" -> <fOO>
    (?i:f)oo         "FOO" -> No matches.
    (?i)a(?-i)b      "AB" -> No matches.
    (a(?i)b|c)       "x C" -> x <C>
    (?i)[^a]         "[^A]a" -> <[><^>A<]>a
    (?s)a.b          "a\nb" -> <a
    b>
    (?x) a  b # comment "ab" -> <ab>
    (?x) a +         "aa" -> <aa>
    |}]

let%expect_test "regex_syntax_groups" =
  show ~source:"xax" {|(?P<n>a)|};
  show ~source:"xax" {|(?<n>a)|};
  show ~source:"xax" {|(?'n'a)|};
  show ~source:"xax" {|(?:a)|};
  show ~source:"ab" {|a(?#comment)b|};
  show ~source:"abcd" {|(a|ab)(c|bcd)|};
  show ~source:"ab" {|a|ab|};
  [%expect
    {|
    (?P<n>a)         "xax" -> x<a>x
    (?<n>a)          "xax" -> x<a>x
    (?'n'a)          "xax" -> x<a>x
    (?:a)            "xax" -> x<a>x
    a(?#comment)b    "ab" -> <ab>
    (a|ab)(c|bcd)    "abcd" -> <abcd>
    a|ab             "ab" -> <a>b
    |}]

let%expect_test "regex_syntax_quantifiers" =
  show ~source:"aaaa" {|a{2,3}|};
  show ~source:"aaaa" {|a{2}|};
  show ~source:"aaaa" {|a{2,}|};
  show ~source:"aaa" {|a{1,2}?|};
  show ~source:"aaa" {|a+?|};
  show ~source:"a{" {|a{|};
  show ~source:"a{,3}" {|a{,3}|};
  show ~source:"aaaaaa" {|a{2}{3}|};
  show ~source:"xb" {|a{0}b|};
  show ~source:"aab" {|(a*)*b|};
  [%expect
    {|
    a{2,3}           "aaaa" -> <aaa>a
    a{2}             "aaaa" -> <aa><aa>
    a{2,}            "aaaa" -> <aaaa>
    a{1,2}?          "aaa" -> <a><a><a>
    a+?              "aaa" -> <a><a><a>
    a{               "a{" -> <a{>
    a{,3}            "a{,3}" -> <a{,3}>
    a{2}{3}          "aaaaaa" -> <aaaaaa>
    a{0}b            "xb" -> x<b>
    (a*)*b           "aab" -> <aab>
    |}]

let%expect_test "regex_syntax_escapes" =
  show ~source:"xAx" {|\x41|};
  show ~source:"xAx" {|\x{41}|};
  show ~source:"xAx" {|\101|};
  show ~source:"a  b" {|\h+|};
  show ~source:"a.b axb" {|\Qa.b\E|};
  show ~source:"a.bb" {|\Qa.b\E+|};
  show ~source:"a-b" {|\-|};
  show ~source:"ab" {|[\x61]|};
  show ~source:"ab" {|[\x61-\x62]+|};
  show ~source:"a-b" {|[\w-]+|};
  show ~source:"1a" {|[[:^digit:]]+|};
  [%expect
    {|
    \x41             "xAx" -> x<A>x
    \x{41}           "xAx" -> x<A>x
    \101             "xAx" -> x<A>x
    \h+              "a  b" -> a<  >b
    \Qa.b\E          "a.b axb" -> <a.b> axb
    \Qa.b\E+         "a.bb" -> <a.bb>
    \-               "a-b" -> a<->b
    [\x61]           "ab" -> <a>b
    [\x61-\x62]+     "ab" -> <ab>
    [\w-]+           "a-b" -> <a-b>
    [[:^digit:]]+    "1a" -> 1<a>
    |}]

let%expect_test "regex_syntax_control_character_escapes" =
  let matches source pattern =
    Format.printf
      "%-6s %S -> %b@."
      pattern
      source
      (Re.execp (Re.compile (Matchers.Regex_syntax.parse pattern)) source)
  in
  matches "\x01" {|\cA|};
  matches "\x00" {|\0|};
  matches "\x0b" {|\v|};
  matches "\r\n" {|\R|};
  matches "\x08" {|[\b]|};
  [%expect
    {|
    \cA    "\001" -> true
    \0     "\000" -> true
    \v     "\011" -> true
    \R     "\r\n" -> true
    [\b]   "\b" -> true
    |}]

let%expect_test "regex_syntax_rejects_with_a_reason" =
  let show_rejection pattern =
    match Matchers.Regex_syntax.parse pattern with
    | _ -> Format.printf "%s: accepted@." pattern
    | exception Matchers.Regex_syntax.Invalid message -> Format.printf "%s@." message
  in
  List.iter
    ~f:show_rejection
    [ {|foo(?=bar)|}
    ; {|foo(?!bar)|}
    ; {|(?<=x)a|}
    ; {|(?<!x)a|}
    ; {|(a)\1|}
    ; {|(?P=n)|}
    ; {|a++|}
    ; {|(?>a+)|}
    ; {|(a)(?1)|}
    ; {|\p{L}|}
    ; {|(*UTF8)a|}
    ; {|(|}
    ; {|a)|}
    ; {|*a|}
    ; {|a**|}
    ; {|[a|}
    ; {|[z-a]|}
    ; {|[[:nope:]]|}
    ; {|a{3,2}|}
    ; {|a{70000}|}
    ; {|\q|}
    ; {|\|}
    ; {|(?U)a|}
    ];
  [%expect
    {|
    invalid regex "foo(?=bar)": lookahead is not supported
    invalid regex "foo(?!bar)": lookahead is not supported
    invalid regex "(?<=x)a": lookbehind is not supported
    invalid regex "(?<!x)a": lookbehind is not supported
    invalid regex "(a)\\1": backreferences are not supported
    invalid regex "(?P=n)": backreferences are not supported
    invalid regex "a++": possessive quantifiers are not supported (they require backtracking control)
    invalid regex "(?>a+)": atomic groups are not supported
    invalid regex "(a)(?1)": recursion is not supported
    invalid regex "\\p{L}": Unicode properties are not supported
    invalid regex "(*UTF8)a": backtracking verbs like (*VERB) are not supported
    invalid regex "(": missing ) to close group
    invalid regex "a)": unmatched )
    invalid regex "*a": quantifier does not follow a repeatable item
    invalid regex "a**": quantifier does not follow a repeatable item
    invalid regex "[a": missing ] to close character class
    invalid regex "[z-a]": range out of order in character class: z-a
    invalid regex "[[:nope:]]": unknown POSIX class [:nope
    invalid regex "a{3,2}": numbers out of order in {3,2}
    invalid regex "a{70000}": repetition count is too large
    invalid regex "\\q": unknown escape \q
    invalid regex "\\": \ at end of pattern
    invalid regex "(?U)a": unsupported flag (?U)
    |}]

let%expect_test "regex_syntax_word_boundaries_are_latin1" =
  (* \b comes from Re, which considers the leading UTF-8 byte of é a word
     character, so there is no boundary between f and é. Unlike PCRE, which matched
     <caf>é, we leave the word alone. *)
  show ~source:"café x" {|\b\w+\b|};
  [%expect {|\b\w+\b          "caf\195\169 x" -> café <x>|}]
