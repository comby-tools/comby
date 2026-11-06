open Vangstrom

type t = Re.re

let make pattern = Re.compile (Re.seq [ Re.start; Regex_syntax.parse pattern ])

(* Re matches strings, but the parser's buffer is a bigstring. A hole is tried at
   many positions of one source, so convert the buffer once rather than each try. *)
let source_of_buffer =
  let cached = ref None in
  fun buffer ->
    match !cached with
    | Some (cached_buffer, source) when cached_buffer == buffer -> source
    | _ ->
      let source = Bigstringaf.substring buffer ~off:0 ~len:(Bigstringaf.length buffer) in
      cached := Some (buffer, source);
      source

let regexp rex =
  (* Matching in the whole source, by position, gives \\b and \\B the character to
     the left of the position, and makes ^ and \\A mean the start of the source. *)
  Unsafe.peek 1 (fun buffer ~off ~len:_ ->
    match Re.exec ~pos:off rex (source_of_buffer buffer) with
    | groups -> Some (Re.Group.get groups 0)
    | exception Not_found -> None)
  >>= function
  | Some result -> advance (String.length result) *> return result
  | None -> fail "No match"
