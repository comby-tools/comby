open! Core
open! Async
open! Import


let prev =
  {|
    assert (
      Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
    assert (
      Int.( = )
        (Set.length t.by_varying_usage)
        (Hashtbl.length t.bucket_id_to_keys)))
|}
;;

let next =
  {|
  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
  assert (
    Int.( = ) (Set.length t.by_varying_usage) (Hashtbl.length t.bucket_id_to_keys))
|}
;;

let%expect_test "default" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,7 +1,4(off) ============================================================
    (fg:black) |
    (fg:black bg:green)+|(fg:green)  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
    (fg:black) |(off)  assert (
    (fg:black bg:yellow)!|(off)      Int.( = ) (Set.length t.by_varying_usage) ((fg:red)Set.length t.by_constant_usage));
    (fg:black bg:yellow)!|(fg:red)    assert (
    (fg:black bg:yellow)!|(fg:red)      Int.( = )
    (fg:black bg:yellow)!|(fg:red)        (Set.length t.by_varying_usage)
    (fg:black bg:yellow)!|(fg:red)        ((off)Hashtbl.length t.bucket_id_to_keys(fg:red))(off)))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-no-semantic-cleanup" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-no-semantic-cleanup" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,7 +1,4(off) ============================================================
    (fg:black) |
    (fg:black bg:green)+|(fg:green)  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
    (fg:black) |(off)  assert (
    (fg:black bg:yellow)!|(off)      Int.( = ) (Set.length t.by_varying_usage) ((fg:red)Set.length t.by_constant_usage));
    (fg:black bg:yellow)!|(fg:red)    assert (
    (fg:black bg:yellow)!|(fg:red)      Int.( = )
    (fg:black bg:yellow)!|(fg:red)        (Set.length t.by_varying_usage)
    (fg:black bg:yellow)!|(fg:red)        ((off)Hashtbl.length t.bucket_id_to_keys(fg:red))(off)))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

(* The diff gets much better if I add a newline to both inputs. *)

let prev =
  {|
    assert (
      Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));

    assert (
      Int.( = )
        (Set.length t.by_varying_usage)
        (Hashtbl.length t.bucket_id_to_keys)))
|}
;;

let next =
  {|
  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));

  assert (
    Int.( = ) (Set.length t.by_varying_usage) (Hashtbl.length t.bucket_id_to_keys))
|}
;;

let%expect_test "with extra newlines" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,8 +1,5(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
    (fg:black) |
    (fg:black) |(off)  assert (
    (fg:black bg:yellow)!|(off)      Int.( = )
    (fg:black bg:yellow)!|(off)        (Set.length t.by_varying_usage)
    (fg:black bg:yellow)!|(off)        (Hashtbl.length t.bucket_id_to_keys(fg:red))(off)))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
