type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }
[@@deriving sexp]

val create : ?rewrite_template:string -> ?rule:Rule.t -> match_template:string -> unit -> t
