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

let to_json diff_only replacements path diff result =
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
  if diff_only then
    `Assoc
      [ ("uri", uri)
      ; ("diff", diff)
      ]
  else
    `Assoc
      [ ("uri", uri)
      ; ("rewritten_source", `String result)
      ; ("in_place_substitutions", value)
      ; ("diff", diff)
      ]

let yojson_to_string kind json =
  match kind with
  | `Pretty -> Yojson.Safe.pretty_to_string json
  | `Lines -> Yojson.Safe.to_string json

let pp_json_pretty ppf (source_path, replacements, replacement_content, diff, diff_only) =
  Format.fprintf ppf "%s" @@ yojson_to_string `Pretty @@ to_json diff_only replacements source_path diff replacement_content

let pp_json_pretty_diff_only ppf (source_path, replacements, replacement_content, diff, diff_only) =
  Format.fprintf ppf "%s" @@ yojson_to_string `Pretty @@ to_json diff_only replacements source_path diff replacement_content

let pp_json_line ppf (source_path, replacements, replacement_content, diff, diff_only) =
  Format.fprintf ppf "%s" @@ Yojson.Safe.to_string @@ to_json diff_only replacements source_path diff replacement_content
