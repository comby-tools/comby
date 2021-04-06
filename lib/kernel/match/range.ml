type t =
  { match_start : Location.t [@key "start"]
  ; match_end : Location.t [@key "end"]
  }
[@@deriving yojson, eq, sexp]

let default =
  { match_start = Location.default
  ; match_end = Location.default
  }
