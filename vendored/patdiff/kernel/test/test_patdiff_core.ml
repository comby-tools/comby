open! Core_kernel
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

let%test_module _ =
  (module struct
    let prev : Diff_input.t = { name = "old"; text = "Foo bar buzz" }
    let next : Diff_input.t = { name = "old"; text = "Foo buzz" }

    let%expect_test "Ansi output generates a single line diff" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~split_long_lines:false
           ~produce_unified_lines:true
           ~output:Ansi
           ~prev
           ~next
           ());
      [%expect {|
      -1,1 +1,1
      [0;1;33m!|[0mFoo[0;31m bar[0m buzz |}]
    ;;

    let%expect_test "Ascii is supported if [produce_unified_lines] is false" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~split_long_lines:false
           ~produce_unified_lines:false
           ~output:Ascii
           ~prev
           ~next
           ());
      [%expect {|
      -1,1 +1,1
      -|Foo bar buzz
      +|Foo buzz |}]
    ;;

    let%expect_test "don't highlight empty newlines (ascii)" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~keep_ws:true
           ~split_long_lines:false
           ~produce_unified_lines:false
           ~output:Ascii
           ~prev:{ name = "old"; text = "" }
           ~next:{ name = "new"; text = "\n\n\n" }
           ());
      [%expect {|
        -1,0 +1,3
        +|
        +|
        +| |}]
    ;;

    let%expect_test "don't highlight empty newlines (ansi)" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~keep_ws:true
           ~split_long_lines:false
           ~produce_unified_lines:false
           ~output:Ansi
           ~prev:{ name = "old"; text = "" }
           ~next:{ name = "new"; text = "\n\n\n" }
           ());
      [%expect
        {|
        -1,0 +1,3
        [0;1;32m+|[0m[0;32m[0m
        [0;1;32m+|[0m[0;32m[0m
        [0;1;32m+|[0m[0;32m[0m |}]
    ;;

    let%expect_test "do highlight empty newlines with some spaces (ansi)" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~keep_ws:true
           ~split_long_lines:false
           ~produce_unified_lines:false
           ~output:Ansi
           ~prev:{ name = "old"; text = "" }
           ~next:{ name = "new"; text = "  \n  \n  \n" }
           ());
      [%expect
        {|
        -1,0 +1,3
        [0;1;32m+|[0m[0;7;32m  [0m
        [0;1;32m+|[0m[0;7;32m  [0m
        [0;1;32m+|[0m[0;7;32m  [0m |}]
    ;;

    let%test "Ascii is not supported if [produce_unified_lines] is true" =
      match
        Patdiff_core.patdiff
          ~split_long_lines:false
          ~produce_unified_lines:true
          ~output:Ascii
          ~prev
          ~next
          ()
      with
      | exception _ -> true
      | (_ : string) -> false
    ;;
  end)
;;

let%test_module "python" =
  (module struct
    let prev : Diff_input.t = { name = "old.py"; text = "print(5)" }
    let next : Diff_input.t = { name = "new.py"; text = "if True:\n    print(5)" }
    let doesn't_contain_ansi_escapes s = not (String.contains s '\027')

    let%expect_test "Ansi output generates a single line diff" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~split_long_lines:false
           ~produce_unified_lines:true
           ~output:Ansi
           ~prev
           ~next
           ());
      [%expect
        {|
      -1,1 +1,2
      [0;1;33m!|[0m[0;32mif True:[0m
      [0;1;33m!|[0m[0;7;32m    [0mprint(5) |}]
    ;;

    let%expect_test "Ascii is supported if [produce_unified_lines] is false" =
      printf
        "%s\n"
        (Patdiff_core.patdiff
           ~split_long_lines:false
           ~produce_unified_lines:false
           ~output:Ascii
           ~prev
           ~next
           ());
      [%expect
        {|
      -1,1 +1,2
      -|print(5)
      +|if True:
      +|    print(5) |}]
    ;;

    let%test _ =
      Patdiff_core.patdiff ~output:Ascii ~produce_unified_lines:false ~prev ~next ()
      |> doesn't_contain_ansi_escapes
    ;;

    let%test _ =
      Patdiff_core.patdiff
        ~output:Ascii
        ~produce_unified_lines:false
        ~keep_ws:false
        ~prev
        ~next
        ()
      |> doesn't_contain_ansi_escapes
    ;;

    let%test _ =
      Patdiff_core.patdiff
        ~output:Ascii
        ~produce_unified_lines:false
        ~keep_ws:true
        ~prev
        ~next
        ()
      |> doesn't_contain_ansi_escapes
    ;;

    let%test _ =
      Patdiff_core.patdiff
        ~output:Ascii
        ~produce_unified_lines:false
        ~rules:(Format.Rules.strip_styles Format.Rules.default)
        ~prev
        ~next
        ()
      |> doesn't_contain_ansi_escapes
    ;;
  end)
;;
