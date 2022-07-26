open Core
open Comby_kernel
open Test_helpers

let configuration = Matchers.Configuration.create ~match_kind:Fuzzy ()

let create (module E : Matchers.Engine.S) syntax =
  let metasyntax =
    Matchers.Metasyntax.{ syntax; identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; aliases = [] }
  in
  Option.value_exn (E.select_with_extension ~metasyntax ".go")

let%expect_test "custom_metasyntax_everything" =
  let matcher = [ Matchers.Metasyntax.Hole (Everything, Delimited (Some "$", None)) ] in
  let source = "simple(test)" in
  run_all_matches (create (module Matchers.Alpha) matcher) source "simple($A)";
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":12,"line":1,"column":13}},"environment":[{"variable":"A","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"simple(test)"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) source "simple($A)";
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":12,"line":1,"column":13}},"environment":[{"variable":"A","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"simple(test)"}]}
|}];
  let source = "(nested(test))" in
  run_all_matches (create (module Matchers.Alpha) matcher) source "($A)";
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":14,"line":1,"column":15}},"environment":[{"variable":"A","value":"nested(test)","range":{"start":{"offset":1,"line":1,"column":2},"end":{"offset":13,"line":1,"column":14}}}],"matched":"(nested(test))"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) source "($A)";
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":14,"line":1,"column":15}},"environment":[{"variable":"A","value":"nested(test)","range":{"start":{"offset":1,"line":1,"column":2},"end":{"offset":13,"line":1,"column":14}}}],"matched":"(nested(test))"}]}
|}];
  let source = "flat stuff yeah" in
  run_all_matches (create (module Matchers.Alpha) matcher) source "flat $A yeah";
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":15,"line":1,"column":16}},"environment":[{"variable":"A","value":"stuff","range":{"start":{"offset":5,"line":1,"column":6},"end":{"offset":10,"line":1,"column":11}}}],"matched":"flat stuff yeah"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) source "flat $A yeah";
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":15,"line":1,"column":16}},"environment":[{"variable":"A","value":"stuff","range":{"start":{"offset":5,"line":1,"column":6},"end":{"offset":10,"line":1,"column":11}}}],"matched":"flat stuff yeah"}]}
|}]

let%expect_test "custom_metasyntax_regex" =
  let matcher = Matchers.Metasyntax.[ Regex ("$", ':', " ") ] in
  let source = "simple(test)" in
  run_all_matches (create (module Matchers.Alpha) matcher) source {|$A:\w+ |};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple"},{"range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"test"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) source {|$A:\w+ |};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple"},{"range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"test"}]}
|}]

let%expect_test "custom_metasyntax_multiple_holes" =
  let matcher =
    Matchers.Metasyntax.
      [ Hole (Everything, Delimited (Some "$", None)); Hole (Alphanum, Delimited (Some "?", None)) ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "simple(bar)" {|$FOO(?BAR)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"BAR","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}},{"variable":"FOO","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple(bar)"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "simple(bar)" {|$FOO(?BAR)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"BAR","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}},{"variable":"FOO","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple(bar)"}]}
|}];
  run_all_matches (create (module Matchers.Alpha) matcher) "foo(bar)" {|?FOO($BAR)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":8,"line":1,"column":9}},"environment":[{"variable":"BAR","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}},{"variable":"FOO","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo(bar)"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "foo(bar)" {|?FOO($BAR)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":8,"line":1,"column":9}},"environment":[{"variable":"BAR","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}},{"variable":"FOO","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo(bar)"}]}
|}];
  let matcher =
    Matchers.Metasyntax.
      [ Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "$$", None))
      ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "foo(bar.baz)" {|$$A|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}},"environment":[{"variable":"A","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo"},{"range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}},"environment":[{"variable":"A","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"bar"},{"range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"baz","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}}}],"matched":"baz"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "foo(bar.baz)" {|$$A|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}},"environment":[{"variable":"A","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo"},{"range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}},"environment":[{"variable":"A","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"bar"},{"range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"baz","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}}}],"matched":"baz"}]}
|}];
  let matcher =
    Matchers.Metasyntax.
      [ Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "$$", None))
      ; Regex ("$", ':', " ")
      ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "foo(bar.baz)" {|$M:\w+ |};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}},"environment":[{"variable":"M","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo"},{"range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}},"environment":[{"variable":"M","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"bar"},{"range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"M","value":"baz","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}}}],"matched":"baz"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "foo(bar.baz)" {|$M:\w+ |};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}},"environment":[{"variable":"M","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo"},{"range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}},"environment":[{"variable":"M","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"bar"},{"range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"M","value":"baz","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}}}],"matched":"baz"}]}
|}];
  (* Expect no matches: Everything parser takes precedence. Allow folding over list to define order. *)
  let matcher =
    Matchers.Metasyntax.
      [ Regex ("$", ':', " ")
      ; Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "$$", None))
      ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "foo(bar.baz)" {|$M:\w+ |};
  [%expect_exact {|No matches.|}];
  run_all_matches (create (module Matchers.Omega) matcher) "foo(bar.baz)" {|$M:\w+ |};
  [%expect_exact {|No matches.|}]

let%expect_test "custom_metasyntax_underscore" =
  let matcher =
    Matchers.Metasyntax.
      [ Hole (Everything, Delimited (Some "$", None)); Hole (Alphanum, Delimited (Some "?", None)) ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "simple(bar)" {|$_(?_)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"_","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple(bar)"}]}
|}];
  (* different because we record _ the first time and don't subsequently for implicit_equals *)
  run_all_matches (create (module Matchers.Omega) matcher) "simple(bar)" {|$_(?_)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"_","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}}],"matched":"simple(bar)"}]}
|}]

let%expect_test "custom_metasyntax_equivalence" =
  let matcher =
    Matchers.Metasyntax.[ Hole (Everything, Delimited (Some "$", None)); Regex ("$", '~', "$") ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "foo(foo)" {|$A($A~\w+$)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":8,"line":1,"column":9}},"environment":[{"variable":"A","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}},{"variable":"A_equal_!@#$000000000011","value":"foo","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"foo(foo)"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "foo(foo)" {|$A($A~\w+$)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":8,"line":1,"column":9}},"environment":[{"variable":"A","value":"foo","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}},{"variable":"A_equal_!@#$000000000012","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo(foo)"}]}
|}]

let%expect_test "custom_metasyntax_definition_order" =
  let matcher =
    Matchers.Metasyntax.[ Regex ("$", '~', "$"); Hole (Everything, Delimited (Some "$", None)) ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "simple(bar)baz" {|$A($B)$C~\w+$|};
  [%expect_exact {|No matches.|}];
  run_all_matches (create (module Matchers.Omega) matcher) "simple(bar)baz" {|$A($B)$C~\w+$|};
  [%expect_exact {|No matches.|}];
  let matcher =
    Matchers.Metasyntax.[ Hole (Everything, Delimited (Some "$", None)); Regex ("$", '~', "$") ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "simple(bar)baz" {|$A($B)$C~\w+$|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":14,"line":1,"column":15}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"B","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}},{"variable":"C","value":"baz","range":{"start":{"offset":11,"line":1,"column":12},"end":{"offset":14,"line":1,"column":15}}}],"matched":"simple(bar)baz"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "simple(bar)baz" {|$A($B)$C~\w+$|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":14,"line":1,"column":15}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"B","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}},{"variable":"C","value":"baz","range":{"start":{"offset":11,"line":1,"column":12},"end":{"offset":14,"line":1,"column":15}}}],"matched":"simple(bar)baz"}]}
|}]

let%expect_test "custom_metasyntax_rewrite_alpha" =
  let syntax =
    Matchers.Metasyntax.
      [ Hole (Everything, Delimited (Some "$", None)); Hole (Alphanum, Delimited (Some "?", None)) ]
  in
  let metasyntax =
    Matchers.Metasyntax.{ syntax; identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; aliases = [] }
  in
  let matcher = Option.value_exn (Matchers.Alpha.select_with_extension ~metasyntax ".go") in
  let specification =
    Matchers.Specification.create ~match_template:"$A(?B)" ~rewrite_template:"??B -> $A$A" ()
  in
  let result = Pipeline.execute matcher ~metasyntax (String "simple(bar)") specification in
  let output =
    match result with
    | Replacement (_, result, _) -> result
    | Matches _ -> "matches"
    | Nothing -> "nothing"
  in
  print_string output;
  [%expect_exact {|?bar -> simplesimple|}];
  let specification =
    Matchers.Specification.create
      ~match_template:"$A(?B)"
      ~rewrite_template:"$id() $id(a) $id(a)"
      ()
  in
  let result = Pipeline.execute matcher ~metasyntax (String "simple(bar)") specification in
  let output =
    match result with
    | Replacement (_, result, _) -> result
    | Matches _ -> "matches"
    | Nothing -> "nothing"
  in
  print_string output;
  [%expect_exact {|1 2 2|}]

let%expect_test "custom_metasyntax_rewrite_omega" =
  let syntax =
    Matchers.Metasyntax.
      [ Hole (Everything, Delimited (Some "$", None)); Hole (Alphanum, Delimited (Some "?", None)) ]
  in
  let metasyntax =
    Matchers.Metasyntax.{ syntax; identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; aliases = [] }
  in
  let matcher = Option.value_exn (Matchers.Omega.select_with_extension ~metasyntax ".go") in
  let specification =
    Matchers.Specification.create ~match_template:"$A(?B)" ~rewrite_template:"??B -> $A$A" ()
  in
  let result = Pipeline.execute matcher ~metasyntax (String "simple(bar)") specification in
  let output =
    match result with
    | Replacement (_, result, _) -> result
    | Matches _ -> "matches"
    | Nothing -> "nothing"
  in
  print_string output;
  [%expect_exact {|?bar -> simplesimple|}];
  let specification =
    Matchers.Specification.create
      ~match_template:"$A(?B)"
      ~rewrite_template:"$id() $id(a) $id(a)"
      ()
  in
  let result = Pipeline.execute matcher ~metasyntax (String "simple(bar)") specification in
  let output =
    match result with
    | Replacement (_, result, _) -> result
    | Matches _ -> "matches"
    | Nothing -> "nothing"
  in
  print_string output;
  [%expect_exact {|3 4 4|}]

let%expect_test "custom_metasyntax_greek_letters" =
  let matcher = Matchers.Metasyntax.[ Hole (Alphanum, Reserved_identifiers [ "α"; "β" ]) ] in
  run_all_matches (create (module Matchers.Alpha) matcher) "simple(bar)" {|α(β)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"α","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"β","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}}],"matched":"simple(bar)"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "simple(bar)" {|α(β)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"α","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"β","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}}],"matched":"simple(bar)"}]}
|}]

let%expect_test "custom_metasyntax_alphanum_test" =
  let matcher =
    Matchers.Metasyntax.
      [ Hole (Alphanum, Delimited (Some "[:", Some ":]"))
      ; Hole (Alphanum, Reserved_identifiers [ "α"; "β" ])
      ]
  in
  run_all_matches (create (module Matchers.Alpha) matcher) "simple(bar)" {|[:A:](α)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"α","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}}],"matched":"simple(bar)"}]}
|}];
  run_all_matches (create (module Matchers.Omega) matcher) "simple(bar)" {|[:A:](α)|};
  [%expect_exact
    {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"α","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}}],"matched":"simple(bar)"}]}
|}]

let%expect_test "custom_metasyntax_rewrite_length" =
  let syntax =
    Matchers.Metasyntax.
      [ Hole (Alphanum, Delimited (Some "[:", Some ":]"))
      ; Hole (Alphanum, Reserved_identifiers [ "α"; "β" ])
      ]
  in
  let metasyntax =
    Matchers.Metasyntax.{ syntax; identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; aliases = [] }
  in
  run
    ~metasyntax
    (create (module Matchers.Alpha) syntax)
    "simple(bar)"
    {|[:A:](α)|}
    {|[:A:].length (α.length)|};
  [%expect_exact {|6 (3)|}];
  run
    ~metasyntax
    (create (module Matchers.Omega) syntax)
    "simple(bar)"
    {|[:A:](α)|}
    {|[:A:].length (α.length)|};
  [%expect_exact {|6 (3)|}]

let%expect_test "custom_metasyntax_test_alias" =
  let aliases =
    Matchers.Metasyntax.
      [ { pattern = "_1"; match_template = ":[x1]"; rule = Some "where :[x1].length == '1'" }
      ; { pattern = "_2"; match_template = ":[x2]"; rule = Some "where :[x2].length == '2'" }
      ; { pattern = "_3"; match_template = ":[x3]"; rule = Some "where :[x3].length == '3'" }
      ]
  in
  (* Need to use default metasyntax because rules don't yet support arbitrary metasyntax *)
  let metasyntax = { Matchers.Metasyntax.default_metasyntax with aliases } in
  let alpha = Option.value_exn (Matchers.Alpha.select_with_extension ~metasyntax ".go") in
  let omega = Option.value_exn (Matchers.Omega.select_with_extension ~metasyntax ".go") in
  run ~metasyntax alpha "foo(a) foo(ab) foo(abc) foo(abcd)" "foo(_2)" "matched";
  [%expect_exact {|foo(a) matched foo(abc) foo(abcd)|}];
  run ~metasyntax omega "foo(a) foo(ab) foo(abc) foo(abcd)" "foo(_2)" "matched";
  [%expect_exact {|foo(a) matched foo(abc) foo(abcd)|}];
  run ~metasyntax alpha "foo(a) foo(ab) foo(abc) foo(abcd)" "foo(_3)" "matched";
  [%expect_exact {|foo(a) foo(ab) matched foo(abcd)|}];
  run ~metasyntax omega "foo(a) foo(ab) foo(abc) foo(abcd)" "foo(_3)" "matched";
  [%expect_exact {|foo(a) foo(ab) matched foo(abcd)|}]
