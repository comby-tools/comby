type match_kind =
  | Exact
  | Fuzzy

type t =
  { match_kind : match_kind
  ; significant_whitespace : bool
  ; disable_substring_matching : bool
  }

let create
    ?(disable_substring_matching = false)
    ?(match_kind = Fuzzy)
    ?(significant_whitespace = false)
    () =
  { match_kind
  ; significant_whitespace
  ; disable_substring_matching
  }
