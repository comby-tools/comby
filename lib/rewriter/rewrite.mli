open Match

type match_context_replacement =
  { range : range
  ; replacement_content : string
  ; environment : environment
  }
[@@deriving yojson]

type result =
  { rewritten_source : string
  ; in_place_substitutions : match_context_replacement list
  }
[@@deriving yojson]

(** if [source] is given, substitute in-place. If not,
    emit result separated by newlines *)
val all
  :  ?source:string
  -> rewrite_template:string
  -> Match.t list
  -> result option
