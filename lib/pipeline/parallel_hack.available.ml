open Core

open Hack_parallel

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let with_scheduler scheduler ~f =
  let result = f scheduler in
  begin
    try Scheduler.destroy scheduler
    with Unix.Unix_error (_,"kill",_) -> Format.printf "UH OH@."; ()
  end;
  result

let try_or_skip f scheduler ~default =
  try f scheduler with End_of_file -> default

let process_interactive ~f paths number_of_workers =
  if debug then Format.printf "[*] Hack_parallel available. Using it.@.";
  let scheduler = Scheduler.create ~number_of_workers () in
  let process_bucket ~init paths = Fold.interactive ~init ~f paths in
  let map acc bucket_of_paths = process_bucket ~init:acc bucket_of_paths in
  let reduce (acc', c') (acc, c) = (List.append acc acc'), (c' + c) in
  let init = ([], 0) in
  let map_reduce ~init ~map ~reduce data scheduler = (* TODO: simplify this *)
    Scheduler.map_reduce scheduler ~init ~map ~reduce data
  in
  let f = map_reduce ~init ~map ~reduce paths in
  with_scheduler scheduler ~f:(try_or_skip f ~default:([], 0))

let process ~f number_of_workers bound_count sources =
  if debug then Format.printf "[*] Hack_parallel available. Using it.@.";
  match sources with
  | `Paths paths ->
    let scheduler = Scheduler.create ~number_of_workers () in
    let map acc bucket_of_paths = Fold.paths ~init:acc ~f bound_count bucket_of_paths in
    let f scheduler = Scheduler.map_reduce scheduler ~init:0 ~map ~reduce:(+) paths in
    with_scheduler scheduler ~f:(try_or_skip f ~default:0)
  | `Zip (zip_file, paths) ->
    let scheduler = Scheduler.create ~number_of_workers () in
    let map acc bucket_of_paths =
      Fold.with_zip zip_file ~f:(fun zip -> Fold.zip_paths ~init:acc ~f zip bucket_of_paths bound_count) in
    let f scheduler = Scheduler.map_reduce scheduler ~init:0 ~map ~reduce:(+) paths in
    with_scheduler scheduler ~f:(try_or_skip f ~default:0)
