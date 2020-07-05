open Core

module Time = Core_kernel.Time_ns.Span

let debug = false

let epsilon_same = 0.05
let epsilon_warn = 0.2

let read_with_timeout read_from_channel =
  let read_from_fd = Unix.descr_of_in_channel read_from_channel in
  let read_from_channel =
    Unix.select
      ~read:[read_from_fd]
      ~write:[]
      ~except:[]
      ~timeout:(`After (Time.of_int_sec 2))
      ()
    |> (fun { Unix.Select_fds.read; _ } -> List.hd_exn read)
    |> Unix.in_channel_of_descr
  in
  let result = In_channel.input_all read_from_channel in
  if debug then Format.printf "Read: %s@." result;
  result

let read_stats command =
  let open Unix.Process_channels in
  if debug then Format.printf "Running: %s@." command;
  let { stderr; _ } = Unix.open_process_full ~env:[||] command in
  read_with_timeout stderr
  |> Yojson.Safe.from_string
  |> Statistics.of_yojson
  |> function
  | Ok statistics -> statistics
  | Error _ -> failwith "Error reading stats from stderr"

let run ~iterations ~command =
  List.fold
    (List.init iterations ~f:ident)
    ~init:Statistics.empty
    ~f:(fun statistics _ -> Statistics.merge (read_stats command) statistics)
  |> fun { total_time; _ } -> total_time /. (Int.to_float iterations)

let bench_diff ~iterations baseline_command new_command =
  let time_baseline = run ~iterations ~command:baseline_command in
  let time_new_command = run ~iterations ~command:new_command in
  if debug then Format.printf "time_baseline: %f@." time_baseline;
  if debug then  Format.printf "time_new: %f@." time_new_command;
  let delta_x = 1.0 /. (time_new_command /. time_baseline) in
  if Float.(delta_x < 1.0) then begin
    let percentage_delta = 1.0 -. delta_x in
    Format.printf "SLOWER: %.4fx of master@." delta_x;
    if Float.(percentage_delta > epsilon_warn) then begin
      Format.printf
        "FAIL: benchmark epsilon exceeded (%.4f > %.4f)@."
        percentage_delta epsilon_warn;
      1
    end
    else begin
      Format.printf "PASS: difference negligible (%.4f)@." percentage_delta;
      0
    end
  end
  else
    let percentage_delta = delta_x -. 1.0 in
    if Float.(percentage_delta > epsilon_same) then begin
      Format.printf "PASS: %.4fx faster@." delta_x;
      0
    end
    else begin
      Format.printf "PASS: difference negligible (%.4f)@." percentage_delta;
      0
    end

let with_temp_dir f =
  let dir = Filename.temp_dir "bench_" "_comby" in
  let result = f dir in
  match Unix.system (Format.sprintf "rm -rf %s" dir) with
  | Ok () ->
    if debug then Format.printf "Successfully removed temp dir@.";
    result
  | Error _ ->
    if debug then Format.printf "Failed to remove temp dir@.";
    result

let with_zip dir f =
  let output_zip = dir ^/ "master.zip" in
  let repo_uri = "https://github.com/comby-tools/comby" in
  let curl_command =
    Format.sprintf "curl -L -o %s %s/zipball/master/ &> /dev/null" output_zip repo_uri
  in
  begin
    match Unix.system curl_command with
    | Ok () ->
      if debug then Format.printf "Download to %s OK@." output_zip;
      (* XXX for some reason Unix.system does not give enough time, and
         we can run into '"/tmp/bench_.tmp.efb1e5_comby/master.zip: No such file or directory"'. This is a hack. Actually test for the file and wait *)
      Unix.sleep 2;
      let result = f output_zip in
      Unix.remove output_zip;
      result
    | Error _ ->
      Unix.remove output_zip;
      Format.eprintf "Failed to execute curl command: %s" curl_command;
      1
  end

let with_master_comby dir f =
  let new_comby = dir ^/ "new_comby" in
  let baseline_comby = dir ^/ "comby" in
  match Unix.system (Format.sprintf "make release && cp $(pwd)/comby %s" new_comby) with
  | Ok () ->
    if debug then Format.printf "new_comby copy OK@.";
    begin
      match
        Unix.system
          (Format.sprintf
             "git clone --depth=50 --branch=master https://github.com/comby-tools/comby.git %s/comby-master && \
              make -C %s/comby-master release && \
              cp %s/comby-master/comby %s" dir dir dir baseline_comby)
      with
      | Ok () ->
        if debug then Format.printf "master comby make and copy OK@.";
        let result = f baseline_comby new_comby in
        Unix.remove baseline_comby;
        Unix.remove new_comby;
        result
      | Error _ ->
        Unix.remove baseline_comby;
        Unix.remove new_comby;
        Format.eprintf "Failed to clone master and build baseline";
        1
    end
  | Error _ ->
    Format.eprintf "Failed to make this release and copy to temp dir";
    1

let zip_bench () =
  let match_template = "let" in
  let rewrite_template = "derp" in
  let go baseline_binary new_binary zip_file =
    let command_args =
      Format.sprintf "'%s' '%s' .ml,.mli -matcher .ml -sequential -stats -zip %s > /dev/null"
        match_template
        rewrite_template
        zip_file
    in
    let baseline_command = Format.sprintf "%s %s" baseline_binary command_args in
    let new_command = Format.sprintf "%s %s" new_binary command_args in
    bench_diff ~iterations:10 baseline_command new_command
  in
  with_temp_dir (fun dir ->
      with_master_comby dir (fun baseline_comby new_comby ->
          with_zip dir (go baseline_comby new_comby)))



let fs_bench =
  ()

let match_only_bench =
  ()

let rewrite_bench =
  ()

let () =
  exit (zip_bench ())
