module Formatting : sig
  type t =
    | Markdown of string * string
    | Text
end

module Context : sig
  type t =
    { lsif_endpoint : string
    ; repository : string
    ; formatting : Formatting.t
    }
end

val hover_at : Context.t -> filepath:string -> line:int -> column:int -> string option
