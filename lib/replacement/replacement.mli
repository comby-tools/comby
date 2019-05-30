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

val empty_result : result

val pp_json_pretty : Format.formatter -> string option * t list * string * string option -> unit

val pp_json_lines : Format.formatter -> string option * t list * string * string option -> unit
