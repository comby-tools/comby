open Language

type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }

val to_regex : t -> string

val create : ?rewrite_template:string -> ?rule:Language.Rule.t -> match_template:string -> unit -> t
