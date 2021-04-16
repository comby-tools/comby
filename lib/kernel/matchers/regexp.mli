module type Regexp_engine_intf = sig
  type t
  type substrings

  val make: string -> t

  val get_substring: substrings -> int -> string option

  val get_all_substrings: substrings -> string array

  val exec: rex:t -> pos:int -> Bytes.t -> substrings option
end

(** Represents character stream right now.
    Compare char stream interface on t and match_regexp descriptions
    in https://sourcegraph.com/github.com/comby-tools/mparser/-/blob/src/mParser_Char_Stream.mli#L102:8
*)
type t

module Make (Regexp : Regexp_engine_intf): sig
  (* do not use this, use regexp. *)
  val match_regexp: t -> int -> Regexp.t -> Regexp.substrings option

  val make_regexp : string -> Regexp.t

  val regexp : Regexp.t -> string Angstrom.t
end

module PCRE : sig
  module Engine : Regexp_engine_intf
  include module type of Make (Engine)
end

module RE : sig
  module Engine : Regexp_engine_intf
  include module type of Make (Engine)
end
