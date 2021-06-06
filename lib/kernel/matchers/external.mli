open Types

include module type of External

val default_external : External.t

val default : (module External.S)

module Default : External.S
