module Match = Match
type match' = Match.t

module Matchers = struct
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
end

module Rule = struct
  open Language
  type t = Rule.t
  type result = Rule.result

  let sat = Rule.Alpha.sat
  let result_env = Rule.Alpha.result_env
  let create = Rule.Alpha.create
  let apply = Rule.Alpha.apply
end
type rule = Rule.t

module Replacement = Replacement
type replacement = Replacement.result

module Rewrite = struct
  include Rewriter.Rewrite
  include Rewriter.Rewrite_template
end

module Pipeline = struct
  module Specification = Configuration.Specification
  type specification = Specification.t
  include Configuration.Command_input
  include Pipeline
end
