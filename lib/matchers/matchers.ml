module Configuration = Configuration
module Matcher = struct
  module type S = Types.Matcher.S
end

module Alpha = Engine.Make(Alpha.Make)
module Omega = Engine.Make(Omega.Make)

module Engine = struct
  module type S = Types.Engine.S
end

module Syntax = Types.Syntax
module Hole = Types.Hole
module Languages = Languages
module Language = struct
  module type S = Types.Language.S
end
module Metasyntax = Metasyntax
