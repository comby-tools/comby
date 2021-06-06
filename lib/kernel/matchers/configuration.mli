type match_kind =
  | Exact
  | Fuzzy

type t =
  { match_kind : match_kind
  ; significant_whitespace : bool
  ; disable_substring_matching : bool
  ; match_newline_toplevel : bool
  ; fresh : unit -> string
  ; substitute_in_place : bool
  }

val create
  :  ?disable_substring_matching:bool
  -> ?match_kind:match_kind
  -> ?significant_whitespace:bool
  -> ?match_newline_toplevel:bool
  -> ?fresh:(unit -> string)
  -> ?substitute_in_place:bool
  -> unit
  -> t
