open Core_kernel

type chunk_range = (Range.t * Range.t list)

type chunk_match =
  { content : string
  ; start : Location.t
  ; ranges : Range.t list
  }
[@@deriving yojson]


let range_chunks source (ranges : chunk_range list) =
  List.map ranges ~f:(fun (cover, ranges) ->
      let first_line =
        match String.rindex_from source cover.match_start.offset '\n' with
        | None -> 0
        | Some x -> x + 1
      in
      let last_line =
        match String.index_from source cover.match_end.offset '\n' with
        | None -> String.length source
        | Some x -> x
      in
      { content = String.slice source first_line last_line
      ; start =
          { offset = first_line
          ; line = cover.match_start.line
          ; column = 1
          }
      ; ranges = List.rev ranges
      })


let to_chunks source l =
  let rec fold_matches acc (l: Range.t list) : chunk_range list =
    match acc, l with
    | _, [] -> acc
    | [], (current :: tl) -> fold_matches [ (current, [ current ]) ] tl
    | ((cover, ranges) :: tl as acc), (current :: rest) ->
      if cover.match_end.line >= current.match_start.line then
        let ranges = current :: ranges in
        let cover =
          if current.match_end.offset > cover.match_end.offset then
            { cover with match_end = current.match_end }
          else
            cover
        in
        fold_matches ((cover, ranges) :: tl) rest
      else
        fold_matches ((current, [ current ]) :: acc) rest
  in
  l
  |> List.sort ~compare:(fun (left : Range.t) right -> Int.compare left.match_start.offset right.match_start.offset)
  |> fold_matches []
  |> range_chunks source
  |> List.rev
