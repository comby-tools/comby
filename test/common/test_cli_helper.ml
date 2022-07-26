open Core
module Time = Core_kernel.Time_ns.Span

let binary_path = "../../../../comby"

let read_with_timeout read_from_channels =
  let read_from_fds = List.map ~f:Unix.descr_of_in_channel read_from_channels in
  let read_from_channels =
    Unix.select
      ~restart:true
      ~read:read_from_fds
      ~write:[]
      ~except:[]
      ~timeout:(`After (Time.of_int_sec 1))
      ()
    |> (fun { Unix.Select_fds.read; _ } -> read)
    |> List.map ~f:Unix.in_channel_of_descr
  in
  List.map read_from_channels ~f:In_channel.input_all |> String.concat ~sep:"\n"

let read_output command =
  let open Unix.Process_channels in
  let { stdout; stderr; _ } =
    Unix.open_process_full ~env:(Array.of_list [ "COMBY_TEST=1" ]) command
  in
  let stdout_result = In_channel.input_all stdout in
  let stderr_result = In_channel.input_all stderr in
  stdout_result ^ stderr_result

let read_expect_stdin_and_stdout command source =
  let open Unix.Process_channels in
  let { stdin; stdout; stderr } =
    Unix.open_process_full ~env:(Array.of_list [ "COMBY_TEST=1" ]) command
  in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let stdout_result = In_channel.input_all stdout in
  let stderr_result = In_channel.input_all stderr in
  stdout_result ^ stderr_result

let read_expect_stderr command source =
  let open Unix.Process_channels in
  let { stdin; stdout; stderr } =
    Unix.open_process_full ~env:(Array.of_list [ "COMBY_TEST=1" ]) command
  in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let _ = In_channel.input_all stdout in
  let stderr_result = In_channel.input_all stderr in
  stderr_result
