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

module Configuration = struct
  include Configuration.Command_input
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


module Pipeline = Pipeline
module Replacement = Replacement
module Rewriter = Rewriter
