open! Core
open! Import

include module type of struct
  include Patdiff_kernel.Patdiff_core
end

include Patdiff_kernel.Patdiff_core.S
