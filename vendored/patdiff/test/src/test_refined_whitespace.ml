open! Core
open! Async
open Import

let patdiff ~prev ~next extra_flags =
  patdiff ~extra_flags:("-keep-whitespace" :: extra_flags) ~prev ~next
;;

let%expect_test "Show added newline at start of input" =
  let%bind () = patdiff ~prev:"bar\n" ~next:"\n bar\n" [] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,2(off) ============================================================
    (fg:black bg:red)-|(off)bar
    (fg:black bg:green)+|
    (fg:black bg:green)+|(fg:green +reverse) (off)bar
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-unrefined works too" =
  let%bind () = patdiff ~prev:"bar\n" ~next:"\n bar\n" [ "-unrefined" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,2(off) ============================================================
    (fg:black bg:red)-|(fg:red)bar
    (fg:black bg:green)+|
    (fg:black bg:green)+|(fg:green) bar
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-ascii works too (it implies -unrefined)" =
  let%bind () = patdiff ~prev:"bar\n" ~next:"\n bar\n" [ "-ascii" ] in
  [%expect
    {|
    ------ prev/file
    ++++++ next/file
    @|-1,1 +1,2 ============================================================
    -|bar
    +|
    +| bar
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "Show leading whitespace" =
  let%bind () = patdiff ~prev:"bar\n" ~next:" bar\n" [] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
    (fg:black bg:red)-|(off)bar
    (fg:black bg:green)+|(fg:green +reverse) (off)bar
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "Show internal whitespace" =
  let%bind () = patdiff ~prev:"foo bar\n" ~next:"foo  bar\n" [] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
    (fg:black bg:red)-|(off)foo(fg:red +reverse) (off)bar
    (fg:black bg:green)+|(off)foo(fg:green +reverse)  (off)bar
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "Show trailing whitespace" =
  let%bind () = patdiff ~prev:"foo\n" ~next:"foo \n" [] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
    (fg:black bg:red)-|(off)foo
    (fg:black bg:green)+|(off)foo(fg:green +reverse)
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
