type index_t

val empty : index_t

val index : source:string -> index_t

val convert_fast : offset:int -> index_t -> int * int

val convert_slow : offset:int -> source:string -> int * int
