open Core

module Time = Core_kernel.Time_ns.Span

let binary_path = "../../../comby"

let read_with_timeout read_from_channel =
  let read_from_fd = Unix.descr_of_in_channel read_from_channel in
  let read_from_channel =
    Unix.select ~read:[read_from_fd] ~write:[] ~except:[] ~timeout:(`After (Time.of_int_sec 5)) ()
    |> (fun { Unix.Select_fds.read; _ } -> List.hd_exn read)
    |> Unix.in_channel_of_descr
  in
  In_channel.input_all read_from_channel

let read_source_from_stdin command source =
  let open Unix.Process_channels in
  let { stdin; stdout ; stderr = _ } = Unix.open_process_full ~env:[||] command in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
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
