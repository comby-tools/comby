type match_kind =
  | Exact
  | Fuzzy

type t =
  { match_kind : match_kind
  ; significant_whitespace : bool
  ; disable_substring_matching : bool
  ; match_newline_toplevel : bool
  ; fresh : unit -> string
  }

let counter =
  let uuid_for_id_counter = ref 0 in
  fun () ->
    uuid_for_id_counter := !uuid_for_id_counter + 1;
    Format.sprintf "!@#$%012d" !uuid_for_id_counter

let create
    ?(disable_substring_matching = false)
    ?(match_kind = Fuzzy)
    ?(significant_whitespace = false)
    ?(match_newline_toplevel = true)
    ?(fresh = counter)
    () =
  { match_kind
  ; significant_whitespace
  ; disable_substring_matching
  ; match_newline_toplevel
  ; fresh
  }
