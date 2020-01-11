open Core

type t =
  { range : Range.t
  ; environment : Environment.t
  ; matched : string
  }
[@@deriving yojson]

let create ?(range = Range.default) () =
  { range
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

let pp_source_path ppf source_path =
  match source_path with
  | Some path -> Format.fprintf ppf "%s:" path
  | None -> Format.fprintf ppf ""

let pp_line_number ppf start_line =
  Format.fprintf ppf "%d:" start_line

let pp ppf (source_path, matches) =
  if matches = [] then
    ()
  else
    let matched =
      List.map matches ~f:(fun { matched; range; _ } ->
          let matched = String.substr_replace_all matched ~pattern:"\n" ~with_:"\\n" in
          let line = range.match_start.line in
          Format.asprintf "%a%a%s" pp_source_path source_path pp_line_number line matched)
      |> String.concat ~sep:"\n"
    in
    Format.fprintf ppf "%s@." matched

let pp_match_count ppf (source_path, matches) =
  let l = List.length matches in
  if l > 1 then
    Format.fprintf ppf "%a%d matches\n" pp_source_path source_path (List.length matches)
  else if l = 1 then
    Format.fprintf ppf "%a%d match\n" pp_source_path source_path (List.length matches)

let pp_json_lines ppf (source_path, matches) =
  Format.fprintf ppf "%s" @@ Yojson.Safe.to_string @@ to_json source_path matches
