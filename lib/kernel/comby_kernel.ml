module Match = Match

type match' = Match.t

module Replacement = Replacement

type replacement = Replacement.result

module Matchers = struct
  module Language = Matchers.Language
  module Matcher = Matchers.Matcher
  module Configuration = Matchers.Configuration

  type configuration = Configuration.t

  module Hole = Matchers.Hole
  module Metasyntax = Matchers.Metasyntax

  type metasyntax = Matchers.Metasyntax.t

  module External = Matchers.External
  module Languages = Matchers.Languages
  module Template = Matchers.Template
  module Ast = Matchers.Ast
  include Matchers.Matcher_engine

  module Rule = struct
    include Matchers.Rule
    include Matchers.Evaluate
  end

  type rule = Rule.t

  module Specification = Matchers.Specification

  type specification = Specification.t

  module Rewrite = Matchers.Rewriter
end
