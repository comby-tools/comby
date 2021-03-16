module Matchers : sig
  module Configuration : module type of Matchers.Configuration



  module Alpha : sig
    include module type of Matchers.Alpha
  end

(*
  module Configuration = Configuration
  module Syntax = Syntax

  module Hole : sig
    type sort =
      | Everything
      | Expression
      | Alphanum
      | Non_space
      | Line
      | Blank
      | Regex
  end

  module Metasyntax : sig
    type hole_definition =
        Delimited of string option * string option

    type hole_syntax =
      | Hole of Hole.sort * hole_definition
      | Regex of string * char * string

    type t =
      { syntax : hole_syntax list
      ; identifier : char -> bool
      }

    module type S = sig
      val syntax : hole_syntax list
      val identifier : char -> bool
    end

    val default_metasyntax : t

    val create : t -> (module S)

    val default : (module S)

    module Default : S
  end

  module Alpha : sig
    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Metasyntax.S) option
  end

  module Omega = Omega
  module Languages = Languages
*)
end

module Language = Language
module Match = Match
module Replacement = Replacement
module Rewriter = Rewriter
module Server_types = Server_types
module Statistics = Statistics
module Configuration = Configuration
