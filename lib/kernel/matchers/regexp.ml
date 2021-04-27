open Angstrom

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

module type Regexp_engine_intf = sig
  type t
  type substrings

  val make: string -> t

  val get_substring: substrings -> int -> string option

  val get_all_substrings: substrings -> string array

  val exec: rex:t -> pos:int -> Bytes.t -> substrings option
end

type t =
  { buffer_pos : int
  ; buffer : bytes
  }

(* I think I should just implement the analog of string_ for regex with some bounded buffer size. *)

module Make (Regexp: Regexp_engine_intf) = struct
  (* https://sourcegraph.com/github.com/comby-tools/mparser/-/blob/src/mParser_Char_Stream.ml#L231:8 *)
  let match_regexp s pos rex =
    Regexp.exec ~rex ~pos:(pos - s.buffer_pos) s.buffer

  let make_regexp pat =
    Regexp.make pat

  (* TODO: tests and blit thing below *)

  (* FIXME: size. about advance => want to use internal unsafe_apply_opt
     actually. cf. string_ in angstrom.ml. instead, trying "do peek, then
     advance/commit." *)
  let regexp rex : string Angstrom.t =
    (* Why do Unsafe if I can just do peek_string? => So I don't allocate on copy of buffer. *)
    (* But it looks like we can't avoid allocation in converting bigstringaf to bytes *)
    Unsafe.peek 1 (fun buffer ~off ~len:_ -> Bigstringaf.length buffer - off) >>= fun n ->
    Unsafe.peek n (fun buffer ~off ~len ->
        (* This still does a copy :( *)
        let bytes = Bytes.create len in
        Bigstringaf.unsafe_blit_to_bytes buffer ~src_off:off bytes ~dst_off:0 ~len;
        if debug then Format.printf "Matching regex against string: %S@." @@ Bytes.to_string bytes;
        match Regexp.exec ~rex ~pos:0 bytes with
        | None ->
          if debug then Format.printf "None (1)@.";
          None
        | Some substrings ->
          match Regexp.get_substring substrings 0 with
          | None ->
            if debug then Format.printf "None (2)@.";
            None
          | Some result ->
            if debug then Format.printf "Matchy Matchy (3)@.";
            Some (result, String.length result))
    >>= function
    | Some (result, n) ->
      (* if empty string matches, this hole like for optionals (x?), advance 1. *)
      (* we want to advance one so parsing can continue, but if we advance 1 here we will think
         that the match context is at least length 1 and not 0 if this hole is the only thing
         defining the match context *)
      (* let n = if n > 0 then n else 1 in
         advance n >>= fun () -> *)
      if debug then Format.printf "Result indeed: %S len %d@." result n;
      advance n >>= fun () ->
      return result
    | None ->
      fail "No match"
end

module PCRE = struct
  module Engine : Regexp_engine_intf = struct
    type t = Pcre.regexp
    type substrings = Pcre.substrings

    let compile_flags =
      Pcre.cflags [ `ANCHORED ]

    let make pattern =
      Pcre.regexp ~iflags:compile_flags pattern

    let get_substring s idx =
      match Pcre.get_substring s idx with
      | result -> Some result
      | exception Not_found
      | exception Invalid_argument _ -> None

    let get_all_substrings s =
      Pcre.get_substrings s

    let exec ~rex ~pos b =
      match Pcre.exec ~pos ~rex (Bytes.unsafe_to_string b) with
      | result -> Some result
      | exception Not_found -> None
  end

  include Make(Engine)
end

module RE = struct
  module Engine : Regexp_engine_intf = struct
    type t = Re.re
    type substrings = Re.substrings

    let compile_flags =
      [ `Anchored ]

    let make pattern =
      Re.Perl.(compile (re ~opts:compile_flags pattern))

    let get_substring s idx =
      match Re.get s idx with
      | result -> Some result
      | exception Not_found -> None

    let get_all_substrings s =
      Re.get_all s

    let exec ~rex ~pos b =
      match Re.exec ~pos rex (Bytes.unsafe_to_string b) with
      | result -> Some result
      | exception Not_found -> None
  end

  include Make(Engine)
end
