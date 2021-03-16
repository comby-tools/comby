open Core

open Matchers
open Match

open Ast

type t = Ast.t

type result = bool * environment option

type options = Options.t

module type Engine = sig
  val options : t -> Options.t

  val sat : result -> bool

  val result_env : result -> environment option

  val create : string -> expression list Or_error.t

  val apply
    :  ?matcher:(module Matcher.S)
    -> ?substitute_in_place:bool
    -> t
    -> environment
    -> result
end
