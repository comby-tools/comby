open Core

open Match

type t =
  { range : range
  ; replacement_content : string
  ; environment : environment
  }
[@@deriving yojson]

type result =
  { rewritten_source : string
  ; in_place_substitutions : t list
  }
[@@deriving yojson]

let empty_result =
  { rewritten_source = ""
  ; in_place_substitutions = []
  }
[@@deriving yojson]

let to_json replacements path diff result =
  let value = `List (List.map ~f:to_yojson replacements) in
  let uri =
    match path with
    | Some path -> `String path
    | None -> `Null
  in
  let diff =
    match diff with
    | Some diff -> `String diff
    | None -> `Null
  in
  `Assoc
    [ ("uri", uri)
    ; ("rewritten_source", `String result)
    ; ("in_place_substitutions", value)
    ; ("diff", diff)
    ]

let get_diff source_path source_content result =
  let open Patdiff_lib in
  let source_path =
    match source_path with
    | Some path -> path
    | None -> "/dev/null"
  in
  let configuration = Diff_configuration.plain () in
  let prev = Patdiff_core.{ name = source_path; text = source_content } in
  let next = Patdiff_core.{ name = source_path; text = result } in
  Compare_core.diff_strings
    ~print_global_header:true
    configuration
    ~prev
    ~next
  |> function
  | `Different diff -> Some diff
  | `Same -> None

let yojson_to_string kind json =
  match kind with
  | `Pretty -> Yojson.Safe.pretty_to_string json
  | `Lines -> Yojson.Safe.to_string json

let pp_json_pretty ppf (source_path, source_content, replacements, replacement_content) =
  let diff = get_diff source_path source_content replacement_content in
  Format.fprintf ppf "%s" @@ yojson_to_string `Pretty @@ to_json replacements source_path diff replacement_content

let pp_json_lines ppf (source_path, source_content, replacements, replacement_content) =
  let diff = get_diff source_path source_content replacement_content in
  Format.fprintf ppf "%s" @@ Yojson.Safe.to_string @@ to_json replacements source_path diff replacement_content

let pp_diff ppf (source_path, source_content, replacement_content) =
  let diff = get_diff source_path source_content replacement_content in
  Option.value_map diff ~default:() ~f:(fun diff -> Format.fprintf ppf "%s@." diff)
