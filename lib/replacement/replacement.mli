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

val to_json
  :  ?path:string
  -> ?replacements:t list
  -> ?rewritten_source:string
  -> diff:string
  -> unit
  -> Yojson.Safe.json

val empty_result : result
