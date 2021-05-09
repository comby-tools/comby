module Match = Match
type match' = Match.t

module Replacement = Replacement
type replacement = Replacement.result

module Matchers = struct

  module Engine = Matchers.Engine
  module Info = Matchers.Info
  module Language = Matchers.Language

  module Matcher = Matchers.Matcher

  module Configuration = Matchers.Configuration
  type configuration = Configuration.t

  module Syntax = Matchers.Syntax
  type syntax = Matchers.Syntax.t
  module Hole = Matchers.Hole

  module Metasyntax = Matchers.Metasyntax
  type metasyntax = Matchers.Metasyntax.t

  module Alpha = Matchers.Alpha
  module Omega = Matchers.Omega

  module Languages = Matchers.Languages

  module Template = Matchers.Template

  module Rule = struct
    include Matchers.Rule
    include Matchers.Rule.Ast
    include Matchers.Rule.Parser
    include Matchers.Evaluate
  end
  type rule = Rule.t

  module Specification = Matchers.Specification
  type specification = Specification.t

  module Rewrite = struct
    include Matchers.Rewriter.Rewrite
    include Matchers.Rewriter.Rewrite_template
  end
end
