module Matchers = struct
  module Matcher = struct
    module type S = Matchers.Matcher.S
  end
  module Configuration = Matchers.Configuration
  module Syntax = Matchers.Syntax
  module Hole = Matchers.Hole
  module Metasyntax = Matchers.Metasyntax

  module Alpha = Matchers.Alpha
  module Omega = Matchers.Omega

  module Languages = Matchers.Languages
end

module Specification = Configuration.Specification


module Pipeline = Pipeline
module Language = Language
module Match = Match
module Replacement = Replacement
module Rewriter = Rewriter
module Server_types = Server_types
module Statistics = Statistics
module Configuration = Configuration
