open! Core_kernel
open! Import

module type S = sig
  (** An output can apply a style to a string and print a list of hunks *)

  module Rule : sig
    val apply : string -> rule:Format.Rule.t -> refined:bool -> string
  end

  val print
    :  print_global_header:bool
    -> file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> print:(string -> unit)
    -> location_style:Format.Location_style.t
    -> Hunks.t
    -> unit
end

module type Output = sig
  module type S = S

  (** Ascii is Ansi with no styles. *)
  type t =
    | Ansi
    | Ascii
    | Html
  [@@deriving compare, sexp]

  val implies_unrefined : t -> bool
end
