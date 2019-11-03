open Core

open Matchers
open Rewriter

let format s =
  let s = String.chop_prefix_exn ~prefix:"\n" s in
  let leading_indentation = Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' ')) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let configuration = Configuration.create ~match_kind:Fuzzy ()

let all ?(configuration = configuration) template source =
  C.all ~configuration ~template ~source

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let%expect_test "comments_in_string_literals_should_not_be_treated_as_comments_by_fuzzy" =
  let source = {|"/*"(x)|} in
  let template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in
  all template source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|"/*"x|}]

let%expect_test "comments_in_string_literals_should_not_be_treated_as_comments_by_fuzzy_go_raw" =
  let source = {|`//`(x)|} in
  let template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|`//`x|}]

let%expect_test "tolerate_unbalanced_stuff_in_string_literals" =
  let template = {|"("|} in
  let source = {|"("|} in
  let matches = all ~configuration template source in
  print_matches matches;
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": 1, "column": 1 },
      "end": { "offset": 3, "line": 1, "column": 4 }
    },
    "environment": [],
    "matched": "\"(\""
  }
]|}]

let%expect_test "base_literal_matching" =
  let source = {|"hello"|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|hello|}]

let%expect_test "base_literal_matching" =
  let source = {|rewrite ("hello") this string|} in
  let match_template = {|rewrite (":[1]") this string|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|hello|}]

let%expect_test "match_string_literals" =
  let source = {|rewrite (".") this string|} in
  let match_template = {|rewrite (":[1]") this string|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|.|}]

let%expect_test "match_string_literals" =
  let source = {|rewrite ("") this string|} in
  let match_template = {|rewrite (":[1]") this string|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {||}]

let%expect_test "match_string_literals" =
  let source = {|"(" match "a""a" this "(" |} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|"(" "a""a" "(" |}]

(* this tests special functionality in non-literal hole parser
   but which must still ignore unbalanced delims within strings *)
let%expect_test "match_string_literals" =
  let source = {|"(" match "(""(" this "(" |} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|"(" "(""(" "(" |}]

let%expect_test "match_string_literals" =
  let source = {|rewrite ("") this string|} in
  let match_template = {|rewrite (:[1]) this string|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|""|}]

let%expect_test "base_literal_matching" =
  let source = {|"("|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|(|}]

let%expect_test "base_literal_matching" =
  let source = {|"(""("|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|((|}]

let%expect_test "base_literal_matching" =
  let source = {|"(""("|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|((|}]

let%expect_test "base_literal_matching" =
  let source = {|"hello world"|} in
  let match_template = {|":[x] :[y]"|} in
  let rewrite_template = {|:[x] :[y]|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|hello world|}]

(* complex test: basically, we are checking that the inside of this literal is only matched by the val b part *)
let%expect_test "base_literal_matching" =
  let source = {|val a = "class = ${String::class}" val b = "not $a"|} in
  let match_template = {|":[x]$:[[y]]"|} in
  let rewrite_template = {|(rewritten part: (:[x]) ([y]))|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|val a = "class = ${String::class}" val b = (rewritten part: (not ) ([y]))|}]

let%expect_test "base_literal_matching" =
  let source = {|get("type") rekt ("enabled", True)|} in
  let match_template = {|(":[1]", :[[3]])|} in
  let rewrite_template = {|(rewritten part: (:[1]) (:[3]))|} in
  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|get("type") rekt (rewritten part: (enabled) (True))|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"" this|} in
  let match_template = {|match "\"" this|} in
  let rewrite_template = "" in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {||}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"" this|} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = ":[1]" in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|"\""|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"\"" this|} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = ":[1]" in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|"\"\""|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"(\"" "(\"" this|} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = ":[1]" in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|"\"(\"" "(\""|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"(\"" "(\"" this|} in
  let match_template = {|match ":[1]" ":[2]" this|} in
  let rewrite_template = {|:[1] :[2]|} in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|\"(\" (\"|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match 'sin(gle' 'quo(tes' this|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|match 'sin(gle' 'quo(tes' this|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match '\''|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|match '\''|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match 'asdf'|} in
  let match_template = {|':[1]'|} in
  let rewrite_template = {|:[1]|} in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|match asdf|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match '\''|} in
  let match_template = {|':[1]'|} in
  let rewrite_template = {|:[1]|} in

  all match_template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|match \'|}]


let%expect_test "go_raw_string_literals" =
  let source =
    {|
       x = x
       y = `multi-line
            raw str(ing literal`
       z = `other multi-line
            raw stri(ng literal`
    |}
  in
  let match_template = {|`:[1]`|} in
  let rewrite_template = {|:[1]|} in

  Go.all ~configuration ~source ~template:match_template
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|
       x = x
       y = multi-line
            raw str(ing literal
       z = other multi-line
            raw stri(ng literal
    |}]

let%expect_test "go_raw_string_literals" =
  let source = {|blah `(` quux|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  Go.all ~configuration ~source ~template:match_template
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|blah `(` quux|}]

let%expect_test "match_string_literals" =
  let source = {|`(` match `(``(` this `(` |} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = {|:[1]|} in
  Go.all ~configuration ~template:match_template ~source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|`(` `(``(` `(` |}]


let%expect_test "go_raw_string_literals" =
  let source =
    {|
       x = x
       y = `multi-line
            raw "str"(ing literal`
       z = `other multi-line
            raw '"'\"\\s\\\\\tr\ni(ng literal`
    |}
  in
  let match_template = {|`:[1]`|} in
  let rewrite_template = {|:[1]|} in

  Go.all ~configuration ~source ~template:match_template
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string;
  [%expect_exact {|
       x = x
       y = multi-line
            raw "str"(ing literal
       z = other multi-line
            raw '"'\"\\s\\\\\tr\ni(ng literal
    |}]

let%expect_test "regression_matching_kubernetes" =
  let source = {|"\n" y = 5|} in
  let template = {|y = :[1]|} in
  let rewrite_template = {|:[1]|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|"\n" 5|}]


let%expect_test "match_escaped_any_char" =
  let source = {|printf("hello world\n");|} in
  let template = {|printf(":[1]");|} in
  let rewrite_template = {|:[1]|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|hello world\n|}]

let%expect_test "match_escaped_escaped" =
  let source = {|printf("hello world\n\\");|} in
  let template = {|printf(":[1]");|} in
  let rewrite_template = {|:[1]|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|hello world\n\\|}]

let%expect_test "match_escaped_escaped" =
  let source = {|printf("hello world\n\");|} in
  let template = {|printf(":[1]");|} in
  let rewrite_template = {|:[1]|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "EXPECT SUCCESS");
  [%expect_exact {|EXPECT SUCCESS|}]
