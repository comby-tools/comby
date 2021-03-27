open Types

include module type of Metasyntax

val default_metasyntax : Metasyntax.t

val create : Metasyntax.t -> (module Metasyntax.S)

val default : (module Metasyntax.S)

module Default : Metasyntax.S

val json : Metasyntax.t -> string
