type t
[@@deriving yojson]

val create : unit -> t

val vars : t -> string list

val add : ?range:Range.t -> t -> string -> string -> t

val lookup : t -> string -> string option

val update : t -> string -> string -> t

val lookup_range : t -> string -> Range.t option

val update_range : t -> string -> Range.t -> t

val equal : t -> t -> bool

val merge : t -> t -> t

val copy : t -> t

val exists : t -> string -> bool

val to_string : t -> string
