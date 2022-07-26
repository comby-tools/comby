open Types

module Make (_ : functor (_ : Language.S) (_ : Metasyntax.S) (_ : External.S) -> Matcher.S) :
  Engine.S
