open Core

open Comby_kernel
open Matchers

open Test_helpers

let all ?(configuration = configuration) (module M : Matchers.Matcher.S) template source =
  M.all ~configuration ~template ~source ()

let rewrite_all engine template source rewrite_template =
  all engine template source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT")

let rewrite_all_want_fail_case engine template source rewrite_template =
  all engine ~configuration template source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "EXPECT SUCCESS")

let rewrite_all' engine template source rewrite_template =
  all engine template source
  |> (fun matches -> Option.value_exn (Rewrite.all ~source ~rewrite_template matches))
  |> (fun { rewritten_source; _ } -> rewritten_source)
  |> print_string

let head_match (module M : Matchers.Matcher.S) template source =
  M.all ~configuration ~template ~source ()
  |> function
  | [] -> print_string "No matches."
  | hd :: _ ->
    print_string hd.matched

let%expect_test "comments_in_string_literals_should_not_be_treated_as_comments_by_fuzzy" =
  let source = {|"/*"(x)|} in
  let template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all (module Omega.C) template source rewrite_template;
  [%expect_exact {|"/*"x|}]

let%expect_test "comments_in_string_literals_should_not_be_treated_as_comments_by_fuzzy_go_raw" =
  let source = {|`//`(x)|} in
  let template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all (module Omega.Go) template source rewrite_template;
  [%expect_exact {|`//`x|}]

let%expect_test "tolerate_unbalanced_stuff_in_string_literals" =
  let template = {|"("|} in
  let source = {|"("|} in

  all (module Omega.C) ~configuration template source
  |> print_matches;
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

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|hello|}]


let%expect_test "base_literal_matching" =
  let source = {|rewrite ("hello") this string|} in
  let match_template = {|rewrite (":[1]") this string|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|hello|}]


let%expect_test "match_string_literals" =
  let source = {|rewrite (".") this string|} in
  let match_template = {|rewrite (":[1]") this string|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|.|}]


let%expect_test "match_string_literals" =
  let source = {|rewrite ("") this string|} in
  let match_template = {|rewrite (":[1]") this string|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {||}]


let%expect_test "match_string_literals" =
  let source = {|"(" match "a""a" this "(" |} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|"(" "a""a" "(" |}]


(* this tests special functionality in non-literal hole parser
   but which must still ignore unbalanced delims within strings *)
let%expect_test "match_string_literals" =
  let source = {|"(" match "(""(" this "(" |} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|"(" "(""(" "(" |}]


let%expect_test "match_string_literals" =
  let source = {|rewrite ("") this string|} in
  let match_template = {|rewrite (:[1]) this string|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|""|}]


let%expect_test "base_literal_matching" =
  let source = {|"("|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|(|}]


let%expect_test "base_literal_matching" =
  let source = {|"(""("|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|((|}]


let%expect_test "base_literal_matching" =
  let source = {|"(""("|} in
  let match_template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|((|}]


let%expect_test "base_literal_matching" =
  let source = {|"hello world"|} in
  let match_template = {|":[x] :[y]"|} in
  let rewrite_template = {|:[x] :[y]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|hello world|}]

(* complex test: basically, we are checking that the inside of this literal is only matched by the val b part *)
let%expect_test "base_literal_matching" =
  let source = {|val a = "class = ${String::class}" val b = "not $a"|} in
  let match_template = {|":[x]$:[[y]]"|} in
  let rewrite_template = {|(rewritten part: (:[x]) ([y]))|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|val a = "class = ${String::class}" val b = (rewritten part: (not ) ([y]))|}]


let%expect_test "base_literal_matching" =
  let source = {|get("type") rekt ("enabled", True)|} in
  let match_template = {|(":[1]", :[[3]])|} in
  let rewrite_template = {|(rewritten part: (:[1]) (:[3]))|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|get("type") rekt (rewritten part: (enabled) (True))|}]


let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"" this|} in
  let match_template = {|match "\"" this|} in
  let rewrite_template = "" in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {||}]


let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"" this|} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = ":[1]" in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|"\""|}]


let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"\"" this|} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = ":[1]" in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|"\"\""|}]


let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"(\"" "(\"" this|} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = ":[1]" in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|"\"(\"" "(\""|}]


let%expect_test "rewrite_string_literals_8" =
  let source = {|match "\"(\"" "(\"" this|} in
  let match_template = {|match ":[1]" ":[2]" this|} in
  let rewrite_template = {|:[1] :[2]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|\"(\" (\"|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match 'sin(gle' 'quo(tes' this|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|match 'sin(gle' 'quo(tes' this|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match '\''|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|match '\''|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match 'asdf'|} in
  let match_template = {|':[1]'|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
  [%expect_exact {|match asdf|}]

let%expect_test "rewrite_string_literals_8" =
  let source = {|match '\''|} in
  let match_template = {|':[1]'|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.C) match_template source rewrite_template;
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


  rewrite_all' (module Omega.Go) match_template source rewrite_template;
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

  rewrite_all' (module Omega.Go) match_template source rewrite_template;
  [%expect_exact {|blah `(` quux|}]

let%expect_test "match_string_literals" =
  let source = {|`(` match `(``(` this `(` |} in
  let match_template = {|match :[1] this|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all' (module Omega.Go) match_template source rewrite_template;
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

  rewrite_all' (module Omega.Go) match_template source rewrite_template;
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

  rewrite_all (module Omega.Go) template source rewrite_template;
  [%expect_exact {|"\n" 5|}]


let%expect_test "match_escaped_any_char" =
  let source = {|printf("hello world\n");|} in
  let template = {|printf(":[1]");|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all (module Omega.Go) template source rewrite_template;
  [%expect_exact {|hello world\n|}]

let%expect_test "match_escaped_escaped" =
  let source = {|printf("hello world\n\\");|} in
  let template = {|printf(":[1]");|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all (module Omega.Go) template source rewrite_template;
  [%expect_exact {|hello world\n\\|}]

let%expect_test "match_escaped_escaped" =
  let source = {|printf("hello world\n\");|} in
  let template = {|printf(":[1]");|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all_want_fail_case (module Omega.Go) template source rewrite_template;
  [%expect_exact {|EXPECT SUCCESS|}]

let%expect_test "holes_in_raw_literals" =
  let source = {|
        return expect(
            extensionsController.executeCommand({
                command: 'queryGraphQL',
                arguments: [
                    `
                        query ResolveRepo($repoName: String!) {
                            repository(name: $repoName) {
                                url
                            }
                        }
                    `,
                    { repoName: 'foo' },
                ],
            })
        )
|} in
  let template = {|`:[1]`|} in

  head_match (module Omega.Typescript) template source;
  [%expect_exact {|`
                        query ResolveRepo($repoName: String!) {
                            repository(name: $repoName) {
                                url
                            }
                        }
                    `|}]


let%expect_test "holes_in_raw_literals_partial" =
  let source = {|
        return expect(
            extensionsController.executeCommand({
                command: 'queryGraphQL',
                arguments: [
                    `
                        query ResolveRepo($repoName: String!) {
                            repository(name: $repoName) {
                                url
                            }
                        }
                    `,
                    { repoName: 'foo' },
                ],
            })
        )
|} in
  let template = {|` query ResolveRepo(:[1]) {:[2]} `|} in

  head_match (module Omega.Typescript) template source;
  [%expect_exact {|`
                        query ResolveRepo($repoName: String!) {
                            repository(name: $repoName) {
                                url
                            }
                        }
                    `|}]


let%expect_test "dont_detect_comments_in_strings_with_hole_matcher" =
  let source = {|"// not a comment"|} in
  let template = {|":[1]"|} in
  let rewrite_template = {|:[1]|} in

  rewrite_all (module Omega.Go) template source rewrite_template;
  [%expect_exact {|// not a comment|}]

(* Deactivated: this will conflict with division syntax *)
(*
let%expect_test "match_regex_delimiters" =
  let source = {|/f\/oo/ "/bar/"|} in
  let template = {|/:[1]/|} in
  let rewrite_template = {|:[1]|} in
  Typescript.all ~configuration ~template ~source ()
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "EXPECT SUCCESS");
  [%expect_exact {|f\/oo "/bar/"|}]
*)
