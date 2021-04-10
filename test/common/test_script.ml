

let run input =
  let result =
    match Language.Script.parse input with
    | Ok result -> Language.Script.to_string result
    | Error _ -> "ERROR"
  in
  print_string result

let%expect_test "test_script_basic_sequence" =
  let script = {|:[x] -> :[y] where nested;|} in
  run script;
  [%expect_exact {|((Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ("(Variable y)")))))|}];

  let script = {|:[x] -> :[y] where nested  ;  |} in
  run script;
  [%expect_exact {|((Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ("(Variable y)")))))|}];

  let script = {|:[x] -> :[y] where nested|} in
  run script;
  [%expect_exact {|((Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ("(Variable y)")))))|}];

  let script = {|
      :[x] -> :[y] where nested  ;
      :[x] -> :[y] where nested
  |}
  in
  run script;
  [%expect_exact {|((Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ("(Variable y)"))))
 (Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ("(Variable y)")))))|}]


let%expect_test "test_script_optional_rewrite" =
  let script = {|:[x] where nested|} in
  run script;
  [%expect_exact {|((Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ()))))|}];

  let script = {|:[x] where nested; :[y] where nested|} in
  run script;
  [%expect_exact {|((Spec
  ((match_template "(Variable x)") (rule (((Option nested))))
   (rewrite_template ())))
 (Spec
  ((match_template "(Variable y)") (rule (((Option nested))))
   (rewrite_template ()))))|}]

let%expect_test "test_script_optional_rule" =
  let script = {|:[x]|} in
  run script;
  [%expect_exact {|((Spec ((match_template "(Variable x)") (rule ()) (rewrite_template ()))))|}];

  let script = {|:[x]; :[y]; :[z] -> :[q]|} in
  run script;
  [%expect_exact {|((Spec ((match_template "(Variable x)") (rule ()) (rewrite_template ())))
 (Spec ((match_template "(Variable y)") (rule ()) (rewrite_template ())))
 (Spec
  ((match_template "(Variable z)") (rule ())
   (rewrite_template ("(Variable q)")))))|}]

let%expect_test "test_spec_expressions" =
  let script = {|:[x] -> :[y] where nested or :[y] -> :[t] where nested;|} in
  run script;
  [%expect_exact {|((Exp Or
  ((Spec
    ((match_template "(Variable x)") (rule (((Option nested))))
     (rewrite_template ("(Variable y)"))))
   (Spec
    ((match_template "(Variable y)") (rule (((Option nested))))
     (rewrite_template ("(Variable t)")))))))|}];

  let script = {|:[x] where nested or :[y] -> :[t] where nested;|} in
  run script;
  [%expect_exact {|((Exp Or
  ((Spec
    ((match_template "(Variable x)") (rule (((Option nested))))
     (rewrite_template ())))
   (Spec
    ((match_template "(Variable y)") (rule (((Option nested))))
     (rewrite_template ("(Variable t)")))))))|}]
