(* sequences a list of parsers to one parser, throwing away all results except the last. *)
(*
let sequence_chain (p_list : production Angstrom.t list) : production Angstrom.t =
  List.fold p_list ~init:(return Parsed) ~f:( *>)
*)

(* sequences a list of parsers and accumulates their productions in a list *)
(*
let map_chain (p_list : production Angstrom.t list) : production list Angstrom.t =
  List.fold p_list ~init:(return []) ~f:(fun acc parser ->
      acc >>= fun acc ->
      parser >>= fun result ->
      return (result::acc))
*)


(** TEST *)
(*
open Core

open Matchers
open Template

let%expect_test "matcher" =
  let source = {|a (b c) d|} in
  let match_template = {|a (:[[b]] c) :[[d]]|} in
  let production_parsers = Matcher.run match_template in
  let parser = Matcher.sequence_chain production_parsers in
  let result = Engine.run source parser in
  let s =
    match result with
    | Ok (Matcher.Parsed)
    | Ok (Matcher.Match _)
    | Ok (Matcher.Deferred _) -> "ok"
    | Error _ -> "error, no match";
  in
  print_string s;
  [%expect_exact {|ok|}];

  let result = Template.fold source production_parsers ~init:[] ~f:(fun acc ->
      function
      | Match m -> m::acc
      | _ -> acc)
  in
  let s =
    match result with
    | Ok productions ->
      Yojson.Safe.to_string @@
      `List (List.map productions ~f:Template.Matcher.omega_match_production_to_yojson)
    | Error _ -> "error, no match";
  in
  print_string s;
  [%expect_exact {|[{"offset":8,"identifier":"d","text":"d"},{"offset":3,"identifier":"b","text":"b"}]|}]
*)

(** PRINTER TEST *)
(*
 open Core

open Matchers

let%expect_test "basic" =
  let match_template = {|a:[1]b c|} in
  let result = Template.Printer.run match_template in
  print_string (String.concat result);
  [%expect_exact {|Toplevel: Other: a
,Hole: :[1]
,Other: b
,Spaces:
,Other: c

|}]

let%expect_test "delims" =
  let match_template = {|a (b c) d|} in
  let result = Template.Printer.run match_template in
  print_string (String.concat result);
  [%expect_exact {|Toplevel: Other: a
,Spaces:
,Delim_open: (
Other: b
Spaces:
Other: c
Delim_close: )
,Spaces:
,Other: d

|}]

*)
