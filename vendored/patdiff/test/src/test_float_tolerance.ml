open! Core
open! Async
open Import

let diff prev next tolerance message =
  printf !"====== %{Sexp} ======\n" message;
  patdiff
    ~extra_flags:[ "-float-tol"; Percent.to_string tolerance; "-ascii" ]
    ~prev
    ~next
;;

let test prev next =
  let%bind () = diff prev next (Percent.of_mult 0.) [%message "strict"] in
  let%bind () = diff prev next (Percent.of_mult 0.1) [%message "10%"] in
  Deferred.unit
;;

let%expect_test _ =
  let%bind () = test {|
 foo
 bar
 bax
 baz
|} {|
 foo
 bar
 baz
|} in
  [%expect
    {|
    ====== strict ======
    ------ prev/file
    ++++++ next/file
    @|-1,5 +1,4 ============================================================
     |
     | foo
     | bar
    -| bax
     | baz
    ("Unclean exit" (Exit_non_zero 1))
    ====== 10% ======
    ------ prev/file
    ++++++ next/file
    @|-1,5 +1,4 ============================================================
     |
     | foo
     | bar
    -| bax
     | baz
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test _ =
  let%bind () =
    test
      {|((apples 12345678 23456789))
 (bananas (09:30:00.000000 16:00:00.000000))
 (clementines 10s) (durian 50ms)
 (elderberries 1s) (figs 2s)
 (grapes 2)
 (huckleberries 4096) (i_don't_know_any_fruit_starting_with_i 100ms)
 (jujubes 50ms) (kiwis 1000000)
 (limes 30)
 (mangos 10000)
 (nectarines 1.66667m)
 (oranges 1.66667m) (persimmons 17:15:00.000000)
 (quinces ((size 50000) (shelf_life 50s)))
 (raspberries true)
 (strawberries (are_red Surprisingly_not_always))
 (tamarind ())
 (ugli_fruits 30s))
|}
      {|((apples 12345678 23456788))
 (bananas (09:30:00.000000 15:59:00.000000))
 (clementines 10s) (durian 50ms)
 (elderberries 1s) (figs 2s)
 (grapes 2)
 (huckleberries 8192) (i_don't_know_any_fruit_starting_with_i 100ms)
 (jujubes 50ms) (kiwis 1000000)
 (limes 32)
 (mangos 10000)
 (nectarines 1.667m)
 (oranges 1.66667m) (persimmons 17:15:00.000000)
 (quinces ((size 49000) (shelf_life 50s)))
 (raspberries true)
 (strawberries (are_red Surprisingly_not_always))
 (tamarind ())
 (ugli_fruits 32s))
|}
  in
  [%expect
    {|
    ====== strict ======
    ------ prev/file
    ++++++ next/file
    @|-1,16 +1,16 ============================================================
    -|((apples 12345678 23456789))
    -| (bananas (09:30:00.000000 16:00:00.000000))
    +|((apples 12345678 23456788))
    +| (bananas (09:30:00.000000 15:59:00.000000))
     | (clementines 10s) (durian 50ms)
     | (elderberries 1s) (figs 2s)
     | (grapes 2)
    -| (huckleberries 4096) (i_don't_know_any_fruit_starting_with_i 100ms)
    +| (huckleberries 8192) (i_don't_know_any_fruit_starting_with_i 100ms)
     | (jujubes 50ms) (kiwis 1000000)
    -| (limes 30)
    +| (limes 32)
     | (mangos 10000)
    -| (nectarines 1.66667m)
    +| (nectarines 1.667m)
     | (oranges 1.66667m) (persimmons 17:15:00.000000)
    -| (quinces ((size 50000) (shelf_life 50s)))
    +| (quinces ((size 49000) (shelf_life 50s)))
     | (raspberries true)
     | (strawberries (are_red Surprisingly_not_always))
     | (tamarind ())
    -| (ugli_fruits 30s))
    +| (ugli_fruits 32s))
    ("Unclean exit" (Exit_non_zero 1))
    ====== 10% ======
    ------ prev/file
    ++++++ next/file
    @|-1,16 +1,16 ============================================================
     |((apples 12345678 23456788))
    -| (bananas (09:30:00.000000 16:00:00.000000))
    +| (bananas (09:30:00.000000 15:59:00.000000))
     | (clementines 10s) (durian 50ms)
     | (elderberries 1s) (figs 2s)
     | (grapes 2)
    -| (huckleberries 4096) (i_don't_know_any_fruit_starting_with_i 100ms)
    +| (huckleberries 8192) (i_don't_know_any_fruit_starting_with_i 100ms)
     | (jujubes 50ms) (kiwis 1000000)
     | (limes 32)
     | (mangos 10000)
     | (nectarines 1.667m)
     | (oranges 1.66667m) (persimmons 17:15:00.000000)
     | (quinces ((size 49000) (shelf_life 50s)))
     | (raspberries true)
     | (strawberries (are_red Surprisingly_not_always))
     | (tamarind ())
     | (ugli_fruits 32s))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
