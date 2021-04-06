open Core_kernel

type index_t = int array

let empty = [||]

(* Returns a line map that contains the ending offset (column) for each line in
   a source file, including the newline character. The line map is 1-based both
   in lines, and column offset. *)
let index ~source =
  let total_len = String.length source in
  let num_lines = List.length @@ String.split_on_chars source ~on:['\n'] in
  (* Add one, where a[0] is empty, which is not used, so that everything is 1-based. *)
  let len = num_lines + 1 in
  let a = Array.create ~len Int.max_value in
  let line_index = ref 1 in
  let offset = ref 0 in
  let f char =
    match char with
    | '\n' ->
      offset := !offset + 1;         (* Add one to offset for \n. *)
      a.(!line_index) <- !offset;    (* Add to line count. *)
      line_index := !line_index + 1; (* Do next line. *)
    | _ ->
      if !offset = total_len then   (* If it's the last char and wasn't a newline, record this offset. *)
        a.(!line_index) <- !offset
      else
        offset := !offset + 1
  in
  String.iter source ~f;
  a

let rec binary_search a value low high =
  if high <= low then
    (if value >= a.(low) && value < a.(low+1) then
       low+1
     else
       low)
  else let mid = (low + high) / 2 in
    if a.(mid) > value then
      binary_search a value low (mid - 1)
    else if a.(mid) < value then
      binary_search a value (mid + 1) high
    else
      (* if mid is exactly equal, then return line + 1 *)
      mid + 1

(* Offset is 0 based, line map is 1-based. Output is 1-based for line and col. *)
let convert_fast ~offset index =
  let line = binary_search index offset 1 (Array.length index) in
  let col = if line = 0 || line = 1 then offset + 1 else offset - index.(line - 1) + 1 in
  line, col

let convert_slow ~offset ~source =
  let f (offset, line, col) char =
    match offset, char with
    | 0, _ -> (0, line, col)
    | _, '\n' -> (offset - 1, line + 1, 1)
    | _ -> (offset - 1, line, col + 1)
  in
  let _, line, col = String.fold ~init:(offset, 1, 1) ~f source in
  line, col
