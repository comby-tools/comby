module Configuration = Configuration
module Syntax = Types.Syntax
module type Matcher = Types.Matcher.S

module type Engine = Types.Match_engine.S

module Alpha : Engine
module Omega : Engine

module Languages : module type of Languages
module Metasyntax : module type of Metasyntax
