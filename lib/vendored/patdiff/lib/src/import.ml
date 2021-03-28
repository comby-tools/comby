open! Core
include Composition_infix

include struct
  (* Modules directly exported from Patdiff_kernel *)
  open Patdiff_kernel
  module Ansi_output = Ansi_output
  module Comparison_result = Comparison_result
  module Diff_input = Diff_input
  module File_name = File_name
  module Float_tolerance = Float_tolerance
  module Format = Format
  module Hunks = Hunks
  module Is_binary = Is_binary
  module Output = Output
  module Should_keep_whitespace = Should_keep_whitespace
end

module Patience_diff = Patience_diff_lib.Patience_diff
