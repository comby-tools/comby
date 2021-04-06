let process_interactive ~f paths =
  Fold.interactive ~init:([], 0) ~f paths

let process ~f bound_count = function
  | `Paths paths -> Fold.paths ~init:0 ~f bound_count paths
  | `Zip (zip_file, paths) -> Fold.with_zip zip_file ~f:(fun zip -> Fold.zip_paths ~init:0 ~f zip paths bound_count)
