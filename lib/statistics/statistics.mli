module Time = Time
module Timer = Timer

type t =
  { number_of_files : int
  ; lines_of_code : int
  ; number_of_matches : int
  ; total_time : float
  }
[@@deriving yojson]

val empty : t

val merge : t -> t -> t
