module Time = Time
module Timer = Timer

type t =
  { number_of_files : int
  ; lines_of_code : int
  ; number_of_matches : int
  ; total_time : float
  }
[@@deriving yojson]

let empty =
  { number_of_files = 0
  ; lines_of_code = 0
  ; number_of_matches = 0
  ; total_time = 0.0
  }

let merge
    { number_of_files
    ; lines_of_code
    ; number_of_matches
    ; total_time
    }
    { number_of_files = number_of_files'
    ; lines_of_code = lines_of_code'
    ; number_of_matches = number_of_matches'
    ; total_time = total_time'
    } =
  { number_of_files = number_of_files + number_of_files'
  ; lines_of_code = lines_of_code + lines_of_code'
  ; number_of_matches = number_of_matches + number_of_matches'
  ; total_time = total_time +. total_time'
  }
