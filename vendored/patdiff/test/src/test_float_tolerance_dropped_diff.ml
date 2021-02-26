open! Core
open! Async
open! Import

(* Regression test for a case where we used to drop the first line of this diff. *)

let prev = {|
((foo (1 2))
 (bar 0.5%))
|}

let next = {|
()
|}

let%expect_test "default" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,3 +1,2(off) ============================================================
    (fg:black) |
    (fg:black bg:yellow)!|(off)((fg:red)(foo (1 2))
    (fg:black bg:yellow)!|(fg:red) (bar 0.5%)(off))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-float-tolerance 0x" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-float-tolerance"; "0x" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,3 +1,2(off) ============================================================
    (fg:black) |
    (fg:black bg:yellow)!|(off)((fg:red)(foo (1 2))
    (fg:black bg:yellow)!|(fg:red) (bar 0.5%)(off))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-float-tolerance 0x -no-semantic-cleanup" =
  let%bind () =
    patdiff ~prev ~next ~extra_flags:[ "-float-tolerance"; "0x"; "-no-semantic-cleanup" ]
  in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,3 +1,2(off) ============================================================
    (fg:black) |
    (fg:black bg:yellow)!|(off)((fg:red)(foo (1 2))
    (fg:black bg:yellow)!|(fg:red) (bar 0.5%)(off))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
