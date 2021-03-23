module Configuration = Configuration
module Matcher : sig
  module type S = Types.Matcher.S
end

module Engine : sig
  module type S = Types.Engine.S
end

module Alpha : Engine.S
module Omega : Engine.S

module Syntax : module type of Types.Syntax
module Hole : module type of Types.Hole
module Languages : module type of Languages
module Language : sig
  module type S = Types.Language.S
end
module Metasyntax : module type of Metasyntax
