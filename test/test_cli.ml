open Core

module Time = Core_kernel.Time_ns.Span

let binary_path = "../../../comby"

let read_with_timeout read_from_channel =
  let read_from_fd = Unix.descr_of_in_channel read_from_channel in
  let read_from_channel =
    Unix.select
      ~read:[read_from_fd]
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
  let { stdin; stdout; stderr = _ } = Unix.open_process_full ~env:[||] command in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  read_with_timeout stdout

let read_output command =
  let open Unix.Process_channels in
  let { stdout; _ } = Unix.open_process_full ~env:[||] command in
  read_with_timeout stdout

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
  [%expect_exact {|world|}]

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
  [%expect_exact {|world|}];

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where :[1] != "world"|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c "
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|hello world|}]

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
  [%expect_exact {|hello|}]

let%expect_test "with_rewrite_rule_stdin_default_no_extension" =
  let source = "hello world" in
  let match_template = ":[2] :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where rewrite :[1] { | ":[_]" -> ":[2]" }|} in
  let command_args =
    Format.sprintf "-sequential '%s' '%s' -rule '%s'"
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_source_from_stdin command source
  |> print_string;
  [%expect_exact {|hello|}]

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
  [%expect_exact {|\footnote{\scriptsize \url{https://github.com}}|}]


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
  ]
}|}];

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
        "start": { "offset": 0, "line": 1, "column": 0 },
        "end": { "offset": 5, "line": 1, "column": 5 }
      },
      "environment": [
        {
          "variable": "1",
          "value": "X",
          "range": {
            "start": { "offset": 2, "line": 1, "column": 2 },
            "end": { "offset": 3, "line": 1, "column": 3 }
          }
        }
      ],
      "matched": "a X c"
    },
    {
      "range": {
        "start": { "offset": 6, "line": 1, "column": 6 },
        "end": { "offset": 11, "line": 1, "column": 11 }
      },
      "environment": [
        {
          "variable": "1",
          "value": "Y",
          "range": {
            "start": { "offset": 8, "line": 1, "column": 8 },
            "end": { "offset": 9, "line": 1, "column": 9 }
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
  "diff": "--- main.ml\n+++ main.ml\n@@ -1,1 +1,1 @@\n -hello world\n +world"
}|}]
    )
