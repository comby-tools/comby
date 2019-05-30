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

val pp_json_pretty : Format.formatter -> string option * string * t list * string -> unit

val pp_json_lines : Format.formatter -> string option * string * t list * string -> unit

val pp_diff : Format.formatter -> string option * string * string -> unit
