open! Core
open! Async
open! Import

let test extra_flags =
  let filenames = [ "foo"; "bar"; "baz" ] in
  patdiff_dir
    ~extra_flags:("-ascii" :: extra_flags)
    ~prev:(List.map filenames ~f:(fun path -> path, "prev\n"))
    ~next:(List.map filenames ~f:(fun path -> path, "next\n"))
;;

let%expect_test "patdiff" =
  let%bind () = test [] in
  [%expect
    {|
    ------ prev/bar
    ++++++ next/bar
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ------ prev/baz
    ++++++ next/baz
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ------ prev/foo
    ++++++ next/foo
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "patdiff -exclude" =
  let%bind () = test [ "-exclude"; "bar" ] in
  [%expect
    {|
    ------ prev/baz
    ++++++ next/baz
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ------ prev/foo
    ++++++ next/foo
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "patdiff -include" =
  let%bind () = test [ "-include"; "bar|baz" ] in
  [%expect
    {|
    ------ prev/bar
    ++++++ next/bar
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ------ prev/baz
    ++++++ next/baz
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "patdiff -include -exclude" =
  let%bind () = test [ "-include"; "bar|baz"; "-exclude"; "baz" ] in
  [%expect
    {|
    ------ prev/bar
    ++++++ next/bar
    @|-1,1 +1,1 ============================================================
    -|prev
    +|next
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
