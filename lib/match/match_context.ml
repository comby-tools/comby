type t =
  { range : Range.t
  ; environment : Environment.t
  ; matched : string
  }
[@@deriving yojson]

let create () =
  { range = Range.default
  ; environment = Environment.create ()
  ; matched = ""
  }
