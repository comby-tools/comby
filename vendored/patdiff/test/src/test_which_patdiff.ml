open! Core
open! Async
open! Import

(** Test that we are testing the patdiff in the tree, not /bin/patdiff. *)

let%expect_test "which patdiff" =
  within_temp_dir ~links (fun () ->
    let%bind () = system {|which patdiff | grep -q "$TMPDIR"|} in
    [%expect {| |}])
;;
