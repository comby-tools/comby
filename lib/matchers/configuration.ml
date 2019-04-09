type match_kind =
  | Exact
  | Fuzzy

type t =
  { match_kind : match_kind
  ; significant_whitespace: bool
  }

let create ?(match_kind = Fuzzy) ?(significant_whitespace = false) () =
  { match_kind
  ; significant_whitespace
  }
