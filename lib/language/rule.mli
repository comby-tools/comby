open Core

open Matchers
open Match

open Ast

type result = bool * environment option

val sat : result -> bool

val result_env : result -> environment option

val create : string -> expression list Or_error.t

val apply
  :  ?matcher:(module Matcher)
  ->  ?newline_separated:bool
  -> t
  -> environment
  -> result
