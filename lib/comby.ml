module Matchers = struct
  module Configuration = Matchers.Configuration
  include Matchers.Syntax
  include Matchers.Hole

  module Alpha = Matchers.Alpha
  include Matchers.Omega

  include Matchers.Languages
  include Matchers.Metasyntax
end

module Language = Language
module Match = Match
module Replacement = Replacement
module Rewriter = Rewriter
module Server_types = Server_types
module Statistics = Statistics
module Configuration = Configuration
