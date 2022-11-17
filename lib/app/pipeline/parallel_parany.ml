open Configuration
open Command_input

let process_interactive ~f paths number_of_workers =
  let reduce (acc, c) (path, result) =
    match result with
    | Some rewritten_source, c' -> Interactive.{ path; rewritten_source } :: acc, c + c'
    | None, c' -> acc, c + c'
  in
  let init = [], 0 in
  let map path = path, f ~input:(Path path) ~path:(Some path) in
  Parany.Parmap.parfold ~csize:16 number_of_workers map reduce init paths

let process ~f number_of_workers bound_count sources =
  match sources with
  | `Paths paths ->
    Parany.Parmap.parfold
      ~csize:16
      number_of_workers
      (fun path -> f ~input:(Path path) ~output_path:(Some path))
      ( + )
      0
      paths
  | `Zip (zip_file, paths) ->
    Parany.Parmap.parfold
      ~csize:16
      number_of_workers
      (fun path ->
         Fold.with_zip zip_file ~f:(fun zip ->
             Fold.zip_paths ~init:0 ~f zip [path] bound_count
           )
      )
      ( + )
      0
      paths
