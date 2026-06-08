module Configuration = Configuration
module Languages = Languages
module Metasyntax = Metasyntax
module External = External
module Rule = Rule

(* Only need to expose Types.Ast. module type of to export sexp. *)
module Ast : module type of Types.Ast
module Evaluate = Evaluate
module Matcher_engine : module type of Matcher_engine
include module type of Matcher_engine
module Matcher = Types.Matcher
module Hole = Types.Hole
module Language = Types.Language
module Script : module type of Script
module Specification : module type of Specification

module Template : sig
  include module type of Types.Template
  include module type of Template
end

module Rewriter = Rewrite
