open! Core
open! Async
open! Import

let%expect_test "message when non-ASCII text files differ" =
  let prev =
    {|┌Signals──┐┌Values───┐┌Waves─────────┐
      │clock    ││         ││┌───┐   ┌───┐ │
      │         ││         ││    └───┘   └─│
|}
  in
  let next =
    {|┌Signals──┐┌Values───┐┌Waves─────────┐
      │clock2   ││         ││┌───┐   ┌───┐ │
      │         ││         ││    └───┘   └─│
|}
  in
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-location-style"; "omake" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    File "prev/file", line 2, characters 0-1:
    (fg:black) |(off)┌Signals──┐┌Values───┐┌Waves─────────┐
    (fg:black bg:red)-|(fg:red)      │clock(off)    ││         ││┌───┐   ┌───┐ │
    (fg:black bg:green)+|(fg:green)      │clock2(off)   ││         ││┌───┐   ┌───┐ │
    (fg:black) |(off)      │         ││         ││    └───┘   └─│
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "message when binary files differ" =
  let len = 100 in
  let bytes = Bytes.make len 'z' in
  Bytes.set bytes 50 '\000';
  let prev = Bytes.to_string bytes in
  Bytes.set bytes 51 '\001';
  (* change something *)
  let next = Bytes.to_string bytes in
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-location-style"; "omake" ] in
  [%expect
    {|
    File "prev/file", line 1, characters 0-1:
      File "next/file"
      binary files differ

    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
