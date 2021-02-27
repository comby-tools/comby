open! Core
open! Async
open! Import

(* The patdiff devs don't review the patdiff-git-wrapper script. This test is to warn them
   of obvious mistakes, like silently changing the calling convention without updating the
   script. It does not cover all the code paths that git might exercise. *)

let links =
  ("../../bin/patdiff-git-wrapper", `In_path_as, "patdiff-git-wrapper") :: links
;;

let%expect_test "patdiff-git-wrapper" =
  within_temp_dir ~links (fun () ->
    (* Set up repo with a dirty working directory. *)
    let%bind () = run "git" [ "init"; "-q" ] in
    let%bind () = Writer.save "foo" ~contents:"foo bar baz\n" in
    let%bind () = run "git" [ "add"; "foo" ] in
    let%bind () = run "git" [ "commit"; "-a"; "-m"; "z"; "-q" ] in
    let%bind () = Writer.save "foo" ~contents:"foo baz quux\n" in
    (* Override whatever patdiff config the user has. *)
    let%bind () = run "patdiff" [ "-make-config"; ".patdiff" ] in
    let%bind () = [%expect {|
      Default configuration written to .patdiff |}] in
    Unix.putenv ~key:"HOME" ~data:".";
    (* Standard git diff. *)
    let%bind () = run "git" [ "diff" ] in
    let%bind () =
      [%expect
        {|
      diff --git a/foo b/foo
      index 1aeaedb..434ebd4 100644
      --- a/foo
      +++ b/foo
      @@ -1 +1 @@
      -foo bar baz
      +foo baz quux |}]
    in
    (* Diff according to instructions in the script. *)
    Unix.putenv ~key:"GIT_EXTERNAL_DIFF" ~data:"patdiff-git-wrapper";
    let%bind () = system "git diff | ansicodes visualize -minimize" in
    [%expect
      {|
      (+bold)patdiff -git a/foo b/foo
      (+bold)index 1aeaedb..0000000 100644
      (fg:red)------ (+bold) a/foo
      (fg:green)++++++ (+bold) b/foo
      (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
      (fg:black bg:red)-|(off)foo(fg:red) bar(off) baz
      (fg:black bg:green)+|(off)foo baz(fg:green) quux |}])
;;
