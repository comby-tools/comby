module Match = Match

module Matchers = struct
  module Matcher = Matchers.Matcher
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
module Replacement = Replacement
module Rewriter = Rewriter
module Statistics = Statistics
module Configuration = Configuration
