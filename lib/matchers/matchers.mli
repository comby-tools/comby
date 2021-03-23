module Configuration = Configuration
module Languages : module type of Languages
module Metasyntax : module type of Metasyntax

module Matcher : module type of Types.Matcher
module Engine : module type of Types.Engine

module Alpha : Engine.S
module Omega : Engine.S

module Syntax : module type of Types.Syntax
module Hole : module type of Types.Hole
module Language : module type of Types.Language
