open Match

type t =
  { range : range
  ; replacement_content : string
  ; environment : environment
  }
[@@deriving yojson]

type result =
  { rewritten_source : string
  ; in_place_substitutions : t list
  }
[@@deriving yojson]
