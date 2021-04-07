open! Core_kernel
open! Import

(** Patdiff_format is the home of all the internal representations of the formatting
    that will be applied to the diff. ie. prefixes, suffixes, & valid styles. *)

module Color : sig
  module RGB6 : sig
    (** expected (0 ≤ r, g, b < 6) *)
    type t = private
      { r : int
      ; g : int
      ; b : int
      }
    [@@deriving compare, quickcheck, sexp]

    val create_exn : r:int -> g:int -> b:int -> t
  end

  module Gray24 : sig
    (** expected (0 ≤ level < 24) *)
    type t = private { level : int } [@@deriving compare, sexp]

    val create_exn : level:int -> t
  end

  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Gray
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white
    | RGB6 of RGB6.t
    | Gray24 of Gray24.t
  [@@deriving compare, quickcheck, sexp]

  include Comparable.S with type t := t

  (** [rgb6_exn r g b] and [gray24_exn level] raise if the values are out of bound. *)
  val rgb6_exn : int * int * int -> t

  val gray24_exn : int -> t
end

module Style : sig
  type t =
    | Bold
    | Underline
    | Emph
    | Blink
    | Dim
    | Inverse
    | Hide
    | Reset
    | Foreground of Color.t
    | Fg of Color.t
    | Background of Color.t
    | Bg of Color.t
  [@@deriving compare, quickcheck, sexp]

  include Comparable.S with type t := t
end

(** A rule consists of a styled prefix, a styled suffix, and a style. Rules
    are applied to strings using functions defined in Output_ops. *)
module Rule : sig
  (** An affix is either a prefix or a suffix. *)
  module Affix : sig
    type t = private
      { text : string
      ; styles : Style.t list
      }

    val create : ?styles:Style.t list -> string -> t
    val blank : t
  end

  type t = private
    { pre : Affix.t
    ; suf : Affix.t
    ; styles : Style.t list
    }
  [@@deriving sexp_of]

  (** Rule creation: Most rules have a style, and maybe a prefix. For
      instance, a line_next rule might have a bold "+" prefix and a green
      style. *)
  val create : ?pre:Affix.t -> ?suf:Affix.t -> Style.t list -> t

  val blank : t
  val unstyled_prefix : string -> t
  val strip_styles : t -> t
end

(** Rules are configured in the configuration file.
    Default values are provided in Configuration. *)
module Rules : sig
  type t =
    { line_same : Rule.t
    ; line_prev : Rule.t
    ; line_next : Rule.t
    ; line_unified : Rule.t
    ; word_same_prev : Rule.t
    ; word_same_next : Rule.t
    ; word_same_unified : Rule.t
    ; word_prev : Rule.t
    ; word_next : Rule.t
    ; hunk : Rule.t
    ; header_prev : Rule.t
    ; header_next : Rule.t
    }
  [@@deriving compare, sexp_of]

  val default : t
  val strip_styles : t -> t
end

module Location_style : sig
  type t =
    | Diff
    | Omake
    | None
  [@@deriving bin_io, compare, quickcheck, enumerate, equal, sexp]

  include Stringable.S with type t := t

  val omake_style_error_message_start : file:string -> line:int -> string

  val sprint
    :  t
    -> string Patience_diff.Hunk.t
    -> prev_filename:string
    -> rule:(string -> string)
    -> string
end
