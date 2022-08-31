module Formatting : sig
  type t =
    | Markdown
    | Text
end

module Context : sig
  type t =
    { lsif_endpoint : string
    ; repository : string
    ; revision : string
    ; formatting : Formatting.t
    }
end

val hover_at : Context.t -> filepath:string -> line:int -> column:int -> string option
