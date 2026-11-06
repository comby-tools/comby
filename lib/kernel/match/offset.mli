type index_t

val empty : index_t
val index : source:string -> index_t
val convert_fast : offset:int -> index_t -> int * int
val convert_slow : offset:int -> source:string -> int * int

(** The offset where each line of [source] starts, for use with [convert]. *)
val line_starts : source:string -> int array

(** Same result as [convert_slow], given [line_starts] of the source. *)
val convert : offset:int -> source_length:int -> int array -> int * int
