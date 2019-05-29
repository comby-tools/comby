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

let yojson_to_string kind json =
  match kind with
  | `Pretty -> Yojson.Safe.pretty_to_string json
  | `Lines -> Yojson.Safe.to_string json

let pp_json_pretty ppf (source_path, matches) =
  Format.fprintf ppf "%s" @@ yojson_to_string `Pretty @@ to_json source_path matches

let pp_json_lines ppf (source_path, matches) =
  Format.fprintf ppf "%s" @@ yojson_to_string `Lines @@ to_json source_path matches

let pp_match_result ppf (source_path, matches) =
  let pp_source_path ppf source_path =
    match source_path with
    | Some path -> Format.fprintf ppf " in %s " path
    | None -> Format.fprintf ppf "%s" " "
  in
  (* FIXME *)
  let spec_number = 0 in
  Format.fprintf ppf
    "%d matches%afor spec %d (use -json-pretty for json format)\n"
    (List.length matches)
    pp_source_path source_path
    (spec_number + 1)
