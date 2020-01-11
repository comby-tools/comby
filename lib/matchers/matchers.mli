module Configuration = Configuration
module Syntax = Types.Syntax
module type Matcher = Types.Matcher.S

include module type of Languages

(** Omega only *)
module Template = Template
