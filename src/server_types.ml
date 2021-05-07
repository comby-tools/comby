open Core

open Comby_kernel
open Match

module In = struct
  type substitution_request =
    { rewrite_template : string [@key "rewrite"]
    ; environment : Environment.t
    ; id : int
    }
  [@@deriving yojson]

  type match_request =
    { source : string
    ; match_template : string [@key "match"]
    ; rule : string option [@default None]
    ; language : string [@default "generic"]
    ; id : int
    }
  [@@deriving yojson]

  type rewrite_request =
    { source : string
    ; match_template : string [@key "match"]
    ; rewrite_template : string [@key "rewrite"]
    ; rule : string option [@default None]
    ; language : string [@default "generic"]
    ; substitution_kind : string [@default "in_place"]
    ; id : int
    }
  [@@deriving yojson]
end

module Out = struct

  module Matches = struct
    type t =
      { matches : Match.t list
      ; source : string
      ; id : int
      }
    [@@deriving yojson]

    let to_string =
      Fn.compose Yojson.Safe.pretty_to_string to_yojson

  end

  module Rewrite = struct
    type t =
      { rewritten_source : string
      ; in_place_substitutions : Replacement.t list
      ; id : int
      }
    [@@deriving yojson]

    let to_string =
      Fn.compose Yojson.Safe.pretty_to_string to_yojson

  end

  module Substitution = struct

    type t =
      { result : string
      ; id : int
      }
    [@@deriving yojson]

    let to_string =
      Fn.compose Yojson.Safe.pretty_to_string to_yojson

  end
end
