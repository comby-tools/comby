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

let%expect_test "matcher" =
  let source = {|a (b c) d|} in
  let match_template = {|a (:[[b]] c) d|} in
  let production_parsers = Template.Matcher.run match_template in
  let parser = Template.Matcher.sequence_chain production_parsers in
  let result = Template.Engine.run source parser in
  let s =
    match result with
    | Ok (Template.Matcher.Unit)
    | Ok (Template.Matcher.Match _)
    | Ok (Template.Matcher.Intermediate_hole _) -> "ok"
    | Error _ -> "error, no match";
  in
  print_string s;
  [%expect_exact {|Toplevel: Other: a|}]
