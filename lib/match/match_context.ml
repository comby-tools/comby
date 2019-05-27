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
  let json_matches matches = `List (List.map ~f:(fun x -> to_yojson x) matches) in
  match source_path with
  | None -> `Assoc [("uri", `Null); ("matches", json_matches matches)]
  | Some path -> `Assoc [("uri", `String path); ("matches", json_matches matches)]

let pp_json_pretty ppf (source_path, matches) =
  let f = Yojson.Safe.pretty_to_string in
  let json_string = f @@ to_json source_path matches in
  Format.fprintf ppf "%s" json_string

let pp_json_lines ppf (source_path, matches) =
  let f = Yojson.Safe.to_string in
  let json_string = f @@ to_json source_path matches in
  Format.fprintf ppf "%s" json_string

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
