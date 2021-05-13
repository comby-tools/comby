type t

val create : int -> t
val of_bigstring : off:int -> len:int -> Bigstringaf.t -> t

val feed_string    : t -> off:int -> len:int -> string -> unit
val feed_bigstring : t -> off:int -> len:int -> Bigstringaf.t -> unit
val feed_input : t -> [ `String of string | `Bigstring of Bigstringaf.t ] -> unit

val shift : t -> int -> unit

val for_reading : t -> Bigstringaf.t

type unconsumed =
  { buf : Bigstringaf.t
  ; off : int
  ; len : int }

val unconsumed    : ?shift:int -> t -> unconsumed
val of_unconsumed : unconsumed -> t
