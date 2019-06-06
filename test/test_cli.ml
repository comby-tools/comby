open Core

module Time = Core_kernel.Time_ns.Span

let binary_path = "../../../comby"

let read_with_timeout read_from_channels =
  let read_from_fds = List.map ~f:Unix.descr_of_in_channel read_from_channels in
  let read_from_channel =
    Unix.select
      ~read:read_from_fds
      ~write:[]
      ~except:[]
      ~timeout:(`After (Time.of_int_sec 5))
      ()
    |> (fun { Unix.Select_fds.read; _ } -> List.hd_exn read)
    |> Unix.in_channel_of_descr
  in
  In_channel.input_all read_from_channel

let read_source_from_stdin command source =
  let open Unix.Process_channels in
  let { stdin; stdout; stderr } = Unix.open_process_full ~env:[||] command in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  read_with_timeout [stdout; stderr]

let read_output command =
  let open Unix.Process_channels in
  let { stdout; stderr; _ } = Unix.open_process_full ~env:[||] command in
  read_with_timeout [stdout; stderr]

let%expect_test "json_lines_separates_by_line" =
  let source = "hello world" in
  let match_template = "o" in
  let rewrite_template = "i" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-lines" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|{"uri":null,"rewritten_source":"helli wirld","in_place_substitutions":[{"range":{"start":{"offset":7,"line":-1,"column":-1},"end":{"offset":8,"line":-1,"column":-1}},"replacement_content":"i","environment":[]},{"range":{"start":{"offset":4,"line":-1,"column":-1},"end":{"offset":5,"line":-1,"column":-1}},"replacement_content":"i","environment":[]}],"diff":"--- /dev/null\n+++ /dev/null\n@@ -1,1 +1,1 @@\n-hello world\n+helli wirld"}
|}]

let%expect_test "json_lines_json_pretty_do_not_output_when_diff_null" =
  let source = "hello world" in
  let match_template = "asdf" in
  let rewrite_template = "asdf" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-pretty" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {||}]

let%expect_test "json_lines_do_not_output_when_diff_null" =
  let source = "hello world" in
  let match_template = "asdf" in
  let rewrite_template = "asdf" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-lines" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {||}]

let%expect_test "error_on_zip_and_stdin" =
  let command_args = "-zip x -stdin" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command "none"
  |> print_string;
  [%expect_exact {|No templates specified. Either on the command line, or using -templates <directory-containing-templates>
Next error: -zip may not be used with stdin.
|}]

let%expect_test "warn_on_anonymous_and_templates_flag" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -templates nonexistent" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|Warning: Templates specified on the command line AND using -templates. Ignoring match
      and rewrite templates on the command line and only using those in directories.
Could not read required match file nonexistent/match
|}]

let%expect_test "warn_json_lines_and_json_pretty" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-lines -json-pretty" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|Warning: Both -json-lines and -json-pretty specified. Using -json-pretty.
|}]

let%expect_test "stdin_command" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0m[0;31mhello [0mworld
|}]

let%expect_test "with_match_rule" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where :[1] == "world"|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c "
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0m[0;31mhello [0mworld
|}];

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where :[1] != "world"|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c -stdout"
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect{| hello world |}]

let%expect_test "with_rewrite_rule" =
  let source = "hello world" in
  let match_template = ":[2] :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where rewrite :[1] { | ":[_]" -> ":[2]" }|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c "
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0mhello[0;31m world[0m
|}]

let%expect_test "with_rewrite_rule_stdin_default_no_extension" =
  let source = "hello world" in
  let match_template = ":[2] :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where rewrite :[1] { | ":[_]" -> ":[2]" }|} in
  let command_args =
    Format.sprintf "-sequential '%s' '%s' -rule '%s' -stdin" match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0mhello[0;31m world[0m
|}]

let%expect_test "generic_matcher" =
  let source = {|\footnote{\small \url{https://github.com}}|} in
  let match_template = {|\footnote{\small :[1]}|} in
  let rewrite_template = {|\footnote{\scriptsize :[1]}|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .generic" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;2m\footnote{[0m[0;31m\small[0m[0;2m \url{https://github.com}}[0m[0m
[0;42;30m+|[0m[0m\footnote{[0;32m\scriptsize[0m \url{https://github.com}}[0m
|}]


let%expect_test "json_output_option" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -json-pretty '%s' '%s' -f .c "
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|{
  "uri": null,
  "rewritten_source": "c X a c Y a",
  "in_place_substitutions": [
    {
      "range": {
        "start": { "offset": 6, "line": -1, "column": -1 },
        "end": { "offset": 11, "line": -1, "column": -1 }
      },
      "replacement_content": "c Y a",
      "environment": [
        {
          "variable": "1",
          "value": "Y",
          "range": {
            "start": { "offset": 2, "line": -1, "column": -1 },
            "end": { "offset": 3, "line": -1, "column": -1 }
          }
        }
      ]
    },
    {
      "range": {
        "start": { "offset": 0, "line": -1, "column": -1 },
        "end": { "offset": 5, "line": -1, "column": -1 }
      },
      "replacement_content": "c X a",
      "environment": [
        {
          "variable": "1",
          "value": "X",
          "range": {
            "start": { "offset": 2, "line": -1, "column": -1 },
            "end": { "offset": 3, "line": -1, "column": -1 }
          }
        }
      ]
    }
  ],
  "diff":
    "--- /dev/null\n+++ /dev/null\n@@ -1,1 +1,1 @@\n-a X c a Y c\n+c X a c Y a"
}
|}];

  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -json-pretty -match-only '%s' '%s' -f .c "
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|{
  "uri": null,
  "matches": [
    {
      "range": {
        "start": { "offset": 0, "line": 1, "column": 1 },
        "end": { "offset": 5, "line": 1, "column": 6 }
      },
      "environment": [
        {
          "variable": "1",
          "value": "X",
          "range": {
            "start": { "offset": 2, "line": 1, "column": 3 },
            "end": { "offset": 3, "line": 1, "column": 4 }
          }
        }
      ],
      "matched": "a X c"
    },
    {
      "range": {
        "start": { "offset": 6, "line": 1, "column": 7 },
        "end": { "offset": 11, "line": 1, "column": 12 }
      },
      "environment": [
        {
          "variable": "1",
          "value": "Y",
          "range": {
            "start": { "offset": 8, "line": 1, "column": 9 },
            "end": { "offset": 9, "line": 1, "column": 10 }
          }
        }
      ],
      "matched": "a Y c"
    }
  ]
}|}]

let with_zip f =
  let file = Filename.temp_file "comby_" ".zip" in
  let zip =  Zip.open_out file in
  let entry_name = "main.ml" in
  let entry_content = "hello world" in
  Zip.add_entry entry_content zip entry_name;
  Zip.close_out zip;
  f file;
  Unix.remove file

let%expect_test "patdiff_and_zip" =
  with_zip (fun file ->
      let match_template = ":[2] :[1]" in
      let rewrite_template = ":[1]" in
      let command_args =
        Format.sprintf "'%s' '%s' .ml -sequential -json-pretty -zip %s"
          match_template rewrite_template file
      in
      let command = Format.sprintf "%s %s" binary_path command_args in
      read_output command
      |> print_string;
      [%expect_exact {|{
  "uri": "main.ml",
  "rewritten_source": "world",
  "in_place_substitutions": [
    {
      "range": {
        "start": { "offset": 0, "line": -1, "column": -1 },
        "end": { "offset": 5, "line": -1, "column": -1 }
      },
      "replacement_content": "world",
      "environment": [
        {
          "variable": "1",
          "value": "world",
          "range": {
            "start": { "offset": 0, "line": -1, "column": -1 },
            "end": { "offset": 5, "line": -1, "column": -1 }
          }
        }
      ]
    }
  ],
  "diff": "--- main.ml\n+++ main.ml\n@@ -1,1 +1,1 @@\n-hello world\n+world"
}
|}]
    )

let%expect_test "template_parsing_no_match_template" =
  let source = "hello world" in
  let template_dir = "example" ^/ "templates" ^/ "parse-no-match-template" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|Could not read required match file example/templates/parse-no-match-template/match
|}]

let%expect_test "template_parsing_with_trailing_newline" =
  let source = "hello world" in
  let template_dir = "example" ^/ "templates" ^/ "parse-template-no-trailing-newline" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s -stdout" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect{| hello world |}]

let%expect_test "template_parsing_with_trailing_newline" =
  let source = "hello world" in
  let template_dir = "example" ^/ "templates" ^/ "parse-template-with-trailing-newline" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s -stdout" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect{| hello world |}]

let%expect_test "diff_is_default" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31ma[0m[0;2m X [0m[0;31mc a[0m[0;2m Y [0m[0;31mc[0m[0m
[0;42;30m+|[0m[0m[0;32mc[0m X [0;32ma c[0m Y [0;32ma[0m[0m
|}]

let%expect_test "diff_option" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -diff '%s' '%s' -f .c"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|--- /dev/null
+++ /dev/null
@@ -1,1 +1,1 @@
-a X c a Y c
+c X a c Y a
|}]

let%expect_test "stdout_option" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -stdout '%s' '%s' -f .c"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|c X a c Y a|}]

let%expect_test "only_color_prints_colored_diff" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -color"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31ma[0m[0;2m X [0m[0;31mc a[0m[0;2m Y [0m[0;31mc[0m[0m
[0;42;30m+|[0m[0m[0;32mc[0m X [0;32ma c[0m Y [0;32ma[0m[0m
|}]

let%expect_test "diff_explicit_color" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -diff -color"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31ma[0m[0;2m X [0m[0;31mc a[0m[0;2m Y [0m[0;31mc[0m[0m
[0;42;30m+|[0m[0m[0;32mc[0m X [0;32ma c[0m Y [0;32ma[0m[0m
|}]

let%expect_test "exclude_dir_option" =
  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'main' 'pain' -d %s" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect{| hello world |}];

  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'main' 'pain' -d %s -exclude-dir 'nonexist'" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect{| hello world |}]
