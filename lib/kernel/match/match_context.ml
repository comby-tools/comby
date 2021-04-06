open Core_kernel

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

let update_range f range =
  let open Range in
  let open Location in
  let update_location loc =
    let line, column = f loc.offset in
    { loc with line; column }
  in
  let match_start = update_location range.match_start in
  let match_end = update_location range.match_end in
  { match_start; match_end }

let update_environment f env =
  List.fold (Environment.vars env) ~init:env ~f:(fun env var ->
      let open Option in
      let updated =
        Environment.lookup_range env var
        >>| update_range f
        >>| Environment.update_range env var
      in
      Option.value_exn updated)

let update_match f m =
  let range = update_range f m.range in
  let environment = update_environment f m.environment in
  { m with range; environment }

let convert_offset ~fast ~source match_ =
  let f offset =
    let index =
      if fast then
        Offset.index ~source
      else
        Offset.empty
    in
    if fast then
      Offset.convert_fast ~offset index
    else
      Offset.convert_slow ~offset ~source
  in
  update_match f match_

let update_environment f env =
  List.fold (Environment.vars env) ~init:env ~f:(fun env var ->
      let open Option in
      let updated =
        Environment.lookup env var
        >>| f
        >>| Environment.update env var
      in
      Option.value_exn updated)

let to_json source_path matches =
  let json_matches matches =
    matches
    |> List.map ~f:(fun m ->
        { m with matched = String.escaped m.matched;
                 environment = update_environment String.escaped m.environment })
    |> List.map ~f:to_yojson
    |> fun matches ->
    `List matches
  in
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
  if List.is_empty matches then
    ()
  else
    let matched =
      List.map matches ~f:(fun { matched; range; _ } ->
          let matched = String.substr_replace_all matched ~pattern:"\n" ~with_:"\\n" in
          let matched = String.substr_replace_all matched ~pattern:"\r" ~with_:"\\r" in
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
  Format.fprintf ppf "%s\n" @@ Yojson.Safe.to_string @@ to_json source_path matches
