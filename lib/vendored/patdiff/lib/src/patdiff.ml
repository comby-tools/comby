include struct
  (* Modules directly exported from Import *)
  open Import
  module Ansi_output = Ansi_output
  module Diff_input = Diff_input
  module File_name = File_name
  module Format = Format
  module Hunks = Hunks
  module Output = Output
end

module Compare_core = Compare_core
module Configuration = Configuration
module Patdiff_core = Patdiff_core

module Private = struct
  module Is_binary = Import.Is_binary
  module Should_keep_whitespace = Import.Should_keep_whitespace
end
