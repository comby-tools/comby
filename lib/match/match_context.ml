open Core

type t =
  { range : Range.t
  ; environment : Environment.t
  ; matched : string
  }
[@@deriving yojson]

let create () =
  { range = Range.default
  ; environment = Environment.create ()
  ; matched = ""
  }

let to_json source_path matches =
  let json_matches matches = `List (List.map ~f:to_yojson matches) in
  let uri =
    match source_path with
    | Some path -> `String path
    | None -> `Null
  in
  `Assoc
    [ ("uri", uri)
    ; ("matches", json_matches matches)
    ]

let pp_json_lines ppf (source_path, matches) =
  Format.fprintf ppf "%s" @@ Yojson.Safe.to_string @@ to_json source_path matches

let pp_match_count ppf (source_path, matches) =
  let pp_source_path ppf source_path =
    match source_path with
    | Some path -> Format.fprintf ppf "%s:" path
    | None -> Format.fprintf ppf ""
  in
  Format.fprintf ppf "%a%d matches\n" pp_source_path source_path (List.length matches)
