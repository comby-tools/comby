module Configuration = Configuration
module type Matcher = Types.Matcher.S

module type Engine = Types.Match_engine.S

module Alpha : Engine
module Omega : Engine

module Syntax : module type of Types.Syntax
module Hole : module type of Types.Hole
module Languages : module type of Languages
module Metasyntax : module type of Metasyntax
