open! Core
open! Async
open Import

let aansi = "\027[0;1m\n"
let bansi = "\027[0;2m\n"

let%expect_test "ansi escape code in input" =
  let%bind () = patdiff ~extra_flags:[] ~prev:aansi ~next:bansi in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
    (fg:black bg:red)-|
    (fg:black bg:green)+|
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let acolored_text = "\027[0;33myellow text\027[0m\n"
let bcolored_text = "\027[0;34mblue text\027[0m\n"

let%expect_test "colored text" =
  let%bind () = patdiff ~extra_flags:[] ~prev:acolored_text ~next:bcolored_text in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
    (fg:black bg:red)-|(fg:yellow)yellow(off) text
    (fg:black bg:green)+|(fg:blue)blue(off) text
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
