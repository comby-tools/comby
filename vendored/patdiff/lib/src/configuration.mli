(** This module extends {!Patdiff_kernel.Configuration} with an on-disk config format and
    functions to parse them. *)

open! Core
open! Import

include module type of struct
  include Patdiff_kernel.Configuration
end

val load : ?quiet_errors:bool -> string -> t option
val dark_bg : t Lazy.t
val light_bg : t Lazy.t

module On_disk : sig
  module V1 : sig
    type t
  end

  module V0 : sig
    type t

    val to_v1 : t -> V1.t
  end

  type t = V1.t [@@deriving sexp]
end

val parse : On_disk.t -> t
val save_default : filename:string -> unit

(** Reads a config from [filename], which by default is [~/.patdiff]. If [~filename:""] is
    passed or [filename] cannot be read, returns [default]. *)
val get_config : ?filename:string -> unit -> t
