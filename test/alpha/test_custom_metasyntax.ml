open Core

let configuration = Matchers.Configuration.create ~match_kind:Fuzzy ()

let create syntax =
  let metasyntax = Matchers.Metasyntax.{ syntax; identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" } in
  Option.value_exn (Matchers.Alpha.select_with_extension ~metasyntax ".go")

let run (module M : Matchers.Matcher.S) source match_template _rewrite_template =
  M.all ~configuration ~template:match_template ~source ()
  |> function
  | [] -> print_string "No matches."
  | results -> print_string (Format.asprintf "%a" Match.pp_json_lines (None, results))

let%expect_test "custom_metasyntax_everything" =
  let matcher = create
      [ Hole (Everything, Delimited (Some "$", None))
      ]
  in

  let source = "simple(test)" in
  run matcher source "simple($A)" "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":12,"line":1,"column":13}},"environment":[{"variable":"A","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"simple(test)"}]}
|}];

  let source = "(nested(test))" in
  run matcher source "($A)" "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":14,"line":1,"column":15}},"environment":[{"variable":"A","value":"nested(test)","range":{"start":{"offset":1,"line":1,"column":2},"end":{"offset":13,"line":1,"column":14}}}],"matched":"(nested(test))"}]}
|}];

  let source = "flat stuff yeah" in
  run matcher source "flat $A yeah" "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":15,"line":1,"column":16}},"environment":[{"variable":"A","value":"stuff","range":{"start":{"offset":5,"line":1,"column":6},"end":{"offset":10,"line":1,"column":11}}}],"matched":"flat stuff yeah"}]}
|}]

let%expect_test "custom_metasyntax_regex" =
  let matcher = create
      [ Regex ("$", ':', " ")
      ]
  in
  let source = "simple(test)" in
  run matcher source {|$A:\w+ |} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}},"environment":[{"variable":"A","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple"},{"range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"test"}]}
|}]

let%expect_test "custom_metasyntax_multiple_holes" =
  let matcher = create
      [ Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "?", None))
      ]
  in

  run matcher "simple(bar)" {|$FOO(?BAR)|} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"BAR","value":"bar","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":10,"line":1,"column":11}}},{"variable":"FOO","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple(bar)"}]}
|}];

  run matcher "foo(bar)" {|?FOO($BAR)|} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":8,"line":1,"column":9}},"environment":[{"variable":"BAR","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}},{"variable":"FOO","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo(bar)"}]}
|}];


  let matcher = create
      [ Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "$$", None))
      ]
  in

  run matcher "foo(bar.baz)" {|$$A|} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}},"environment":[{"variable":"A","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo"},{"range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}},"environment":[{"variable":"A","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"bar"},{"range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"A","value":"baz","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}}}],"matched":"baz"}]}
|}];

  (* Expect no matches: Everything parser takes precedence. Allow folding over list to define order. *)
  let matcher = create
      [ Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "$$", None))
      ; Regex ("$", ':', " ")
      ]
  in

  run matcher "foo(bar.baz)" {|$M:\w+ |} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}},"environment":[{"variable":"M","value":"foo","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":3,"line":1,"column":4}}}],"matched":"foo"},{"range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}},"environment":[{"variable":"M","value":"bar","range":{"start":{"offset":4,"line":1,"column":5},"end":{"offset":7,"line":1,"column":8}}}],"matched":"bar"},{"range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"M","value":"baz","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":11,"line":1,"column":12}}}],"matched":"baz"}]}
|}];

  let matcher = create
      [ Regex ("$", ':', " ")
      ; Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "$$", None))
      ]
  in

  run matcher "foo(bar.baz)" {|$M:\w+ |} "";
  [%expect_exact {|No matches.|}]


let%expect_test "custom_metasyntax_underscore" =
  let matcher = create
      [ Hole (Everything, Delimited (Some "$", None))
      ; Hole (Alphanum, Delimited (Some "?", None))
      ]
  in

  run matcher "simple(bar)" {|$_(?_)|} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"_","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple(bar)"}]}
|}]

let%expect_test "custom_metasyntax_rewrite" =
  let syntax =
    let open Matchers.Metasyntax in
    [ Hole (Everything, Delimited (Some "$", None))
    ; Hole (Alphanum, Delimited (Some "?", None))
    ]
  in
  let metasyntax = Matchers.Metasyntax.{ syntax; identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" } in
  let matcher = Option.value_exn (Matchers.Alpha.select_with_extension ~metasyntax ".go") in

  (* KNOWN LIMITATION/BUG: if ? is a prefix it conflicts with ? optional syntax
     for variable names and substitution. Expect should be ?bar here. Remove
     optional syntax. *)
  let specification = Configuration.Specification.create ~match_template:"$A(?B)" ~rewrite_template:"??B -> $A$A" () in
  let result = Pipeline.execute matcher ~metasyntax (String "simple(bar)") specification in
  let output = match result with
    | Replacement (_, result, _) -> result
    | Matches _ -> "matches"
    | Nothing -> "nothing"
  in
  print_string output;
  [%expect_exact {|bar -> simplesimple|}];

  let specification = Configuration.Specification.create ~match_template:"$A(?B)" ~rewrite_template:"$id() $id(a) $id(a)" () in
  let result = Pipeline.execute matcher ~metasyntax (String "simple(bar)") specification in
  let output = match result with
    | Replacement (_, result, _) -> result
    | Matches _ -> "matches"
    | Nothing -> "nothing"
  in
  print_string output;
  [%expect_exact {|1 2 2|}];
