open! Core_kernel
open! Import

(* The choice of 8000 bytes is copied from git:

   https://github.com/git/git/blob/b7bd9486b055c3f967a870311e704e3bb0654e4f/xdiff-interface.c#L201
*)
let prefix_length = 8000
let string s = String.contains s '\000' ~len:(Int.min prefix_length (String.length s))
