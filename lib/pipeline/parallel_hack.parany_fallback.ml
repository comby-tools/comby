open Core

open Configuration
open Command_input

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let process_interactive ~f paths number_of_workers =
  if debug then Format.printf "[*] Hack_parallel unavailable. Using parany.@.";
  let reduce (acc, c) (path, result) =
    match result with
    | Some rewritten_source, c' ->
      Interactive.{path; rewritten_source}::acc, c+c'
    | None, c' ->
      acc, c+c'
  in
  let init = ([], 0) in
  let map path = path, f ~input:(Path path) ~path:(Some path) in
  Parany.Parmap.parfold ~csize:16 number_of_workers map reduce init paths

let process ~f number_of_workers _bound_count sources =
  if debug then Format.printf "[*] Hack_parallel unavailable. Using parany.@.";
  match sources with
  | `Paths paths ->
    Parany.Parmap.parfold ~csize:16 number_of_workers (fun path -> f ~input:(Path path) ~output_path:(Some path)) (+) 0 paths
  | `Zip _ -> failwith "Not supported"
