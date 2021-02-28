open Core
open Camlzip

open Configuration
open Command_input

let paths ~init ~f bound_count paths =
  match bound_count with
  | None -> List.fold paths ~init ~f:(fun count path -> count + f ~input:(Path path) ~output_path:(Some path))
  | Some bound_count ->
    List.fold_until paths ~init ~finish:(fun x -> x) ~f:(fun acc path ->
        if acc > bound_count then
          Stop acc
        else
          Continue (acc + f ~input:(Path path) ~output_path:(Some path)))

let loc_paths paths =
  List.fold paths ~init:0 ~f:(fun acc paths ->
      In_channel.read_lines paths
      |> List.length
      |> (+) acc)

let with_zip zip_file ~f =
  let zip_in = Zip.open_in zip_file in
  let result = f zip_in in
  Zip.close_in zip_in;
  result

let zip_paths ~init ~f zip paths bound_count =
  match bound_count with
  | None ->
    List.fold paths ~init ~f:(fun count ({ Zip.filename; _ } as entry) ->
        let source = Zip.read_entry zip entry in
        count + f ~input:(String source) ~output_path:(Some filename))
  | Some max_count ->
    List.fold_until paths ~init ~finish:(fun x -> x) ~f:(fun count ({ Zip.filename; _ } as entry) ->
        if count > max_count then
          Stop count
        else
          let source = Zip.read_entry zip entry in
          Continue (count + f ~input:(String source) ~output_path:(Some filename)))

let loc_zip zip_file paths =
  with_zip zip_file ~f:(fun zip ->
      List.fold paths ~init:0 ~f:(fun acc entry ->
          let source = Zip.read_entry zip entry in
          acc + (List.length (String.split_lines source))))

let interactive ~init ~f paths =
  List.fold ~init paths ~f:(fun (acc, c) path ->
      match f ~input:(Path path) ~path:(Some path) with
      | Some rewritten_source, c' ->
        Interactive.{ path; rewritten_source }::acc, c+c'
      | None, c' ->
        acc, c+c')
