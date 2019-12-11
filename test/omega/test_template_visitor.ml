open Core

open Matchers

let%expect_test "basic" =
  let match_template = {|a:[1]b c|} in
  let result = Template.Printer.run match_template in
  print_string (String.concat result);
  [%expect_exact {|Other: a
Hole: :[1]
Other: b
Spaces:  
Other: c
|}]

let%expect_test "delims" =
  let match_template = {|a (b c) d|} in
  let result = Template.Printer.run match_template in
  print_string (String.concat result);
  [%expect_exact {|Other: a
Spaces:  
Delim_open: (
	Other: b
	Spaces:  
	Other: c
Delim_close: )
Spaces:  
Other: d
|}];
