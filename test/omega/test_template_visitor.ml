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
