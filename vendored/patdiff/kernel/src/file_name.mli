(** Used to determine which name to use for a file, depending on the operation. *)

open! Core_kernel
open! Import

type t =
  | Real of
      { real_name : string
      ; alt_name : string option
      }
  (** A name corresponding to a real file on disk.  [alt_name] is used to display
      the file name and for file extension heuristics. *)
  | Fake of string (** A name not necessarily corresponding to a real file. *)
[@@deriving compare, equal]

(** The name used to access the file system.  May differ from the name used for
    display. *)
val real_name_exn : t -> string

(** The name used for display.  Also used for file extension heuristics.

    If [t] has an [alt_name], then that is used.  Otherwise, the real name is used. *)
val display_name : t -> string

(** Equivalent to {!display_name}. *)
val to_string_hum : t -> string

(** Append a path component to each of [real_name], [alt_name]. *)
val append : t -> string -> t

val dev_null : t
