module Location : sig
  type t =
    { offset : int
    ; line : int
    ; column : int
    }
  [@@deriving yojson, eq, sexp]

  val default : t
end

type location = Location.t
[@@deriving yojson, eq, sexp]

module Range : sig
  type t =
    { match_start : location [@key "start"]
    ; match_end : location [@key "end"]
    }
  [@@deriving yojson, eq, sexp]

  val default : t
end

type range = Range.t
[@@deriving yojson, eq, sexp]

module Environment : sig
  type t
  [@@deriving yojson, eq]

  val create : unit -> t

  val vars : t -> string list

  val add : ?range:range -> t -> string -> string -> t

  val lookup : t -> string -> string option

  val update : t -> string -> string -> t

  val lookup_range : t -> string -> range option

  val update_range : t -> string -> range -> t

  val furthest_match : t -> int

  val equal : t -> t -> bool

  val copy : t -> t

  val merge : t -> t -> t

  val to_string : t -> string

  val exists : t -> string -> bool
end

type environment = Environment.t
[@@deriving yojson]

type t =
  { range : range
  ; environment : environment
  ; matched : string
  }
[@@deriving yojson]

val create : ?range:range -> unit -> t

val pp : Format.formatter -> string option * t list -> unit

val pp_json_lines : Format.formatter -> string option * t list -> unit

val pp_match_count : Format.formatter -> string option * t list -> unit
