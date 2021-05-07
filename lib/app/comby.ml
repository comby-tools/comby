include Comby_kernel

module Pipeline = struct
  include Configuration.Command_input
  include Pipeline
end

module Regex = Configuration.Regex
