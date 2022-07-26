open Core_kernel
open Range

type chunk_match =
  { content : string
  ; start : Location.t
  ; ranges : Range.t list
  }
[@@deriving yojson]

let slice_source source { match_start = { offset = start; _ }; match_end = { offset = _end; _ } } =
  let open Option in
  let index f o = f source o '\n' in
  let first_line = Option.value ~default:0 (index String.rindex_from start >>| ( + ) 1) in
  let last_line = Option.value ~default:(String.length source) (index String.index_from _end) in
  first_line, String.slice source first_line last_line

let to_range_chunk source (cover, ranges) =
  let offset, content = slice_source source cover in
  { content; start = { offset; line = cover.match_start.line; column = 1 }; ranges }

let compare left right = Int.compare left.match_start.offset right.match_start.offset

let to_chunks ?(threshold = 0) source (l : Match_context.t list) =
  let _threshold = threshold in
  (* FIXME: suppress unused *)
  List.map l ~f:(fun { range; _ } -> range)
  |> List.sort ~compare
  |> function
  | [] -> []
  | hd :: tl ->
    List.fold
      ~init:[ hd, [ hd ] ]
      tl
      ~f:(fun acc current ->
        let cover, ranges, rest =
          match acc with
          | (cover, ranges) :: tl -> cover, ranges, tl
          | _ -> assert false
        in
        if cover.match_end.line >= current.match_start.line then (
          let cover =
            if current.match_end.offset > cover.match_end.offset then
              { cover with match_end = current.match_end }
            else
              cover
          in
          (cover, ranges @ [ current ]) :: rest)
        else
          (current, [ current ]) :: acc)
    |> List.rev_map ~f:(to_range_chunk source)

let to_json source_path matches =
  let json_matches matches =
    matches |> List.map ~f:chunk_match_to_yojson |> fun matches -> `List matches
  in
  let uri =
    match source_path with
    | Some path -> `String path
    | None -> `Null
  in
  `Assoc [ "uri", uri; "matches", json_matches matches ]

let pp_chunk_matches ppf (source_path, matches) =
  Format.fprintf ppf "%s\n" @@ Yojson.Safe.to_string @@ to_json source_path matches
