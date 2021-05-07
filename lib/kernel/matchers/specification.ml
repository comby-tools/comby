open Core_kernel

type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }
[@@deriving sexp]


let create ?rewrite_template ?rule ~match_template () =
  { match_template; rule; rewrite_template }
