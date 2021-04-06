open Core_kernel

open Angstrom

let (|>>) p f =
  p >>= fun x -> return (f x)

module Escapable = struct
  module type S = sig
    val delimiter : string
    val escape : char
  end

  module Make (M : S) = struct
    (* delimiters can be escaped and parsing continues within the string body *)
    let escaped_char_s  =
      any_char

    let char_token_s =
      ((char M.escape *> escaped_char_s >>= fun c -> return (Format.sprintf {|%c%c|} M.escape c))
       <|> (any_char |>> String.of_char)
      )

    let base_string_literal =
      ((string M.delimiter *> (many_till char_token_s (string M.delimiter))
        |>> String.concat)
       >>= fun result ->
       return (Format.sprintf {|%s%s%s|} M.delimiter result M.delimiter)
      )
  end
end

module Raw = struct
  module type S = sig
    val left_delimiter : string
    val right_delimiter : string
  end

  module Make (M : S) = struct
    let char_token_s =
      (any_char |>> String.of_char)

    let base_string_literal =
      ((
        string M.left_delimiter *> (many_till char_token_s (string M.right_delimiter))
        |>> String.concat)
       >>= fun result ->
       return (Format.sprintf {|%s%s%s|} M.left_delimiter result M.right_delimiter)
      )
  end
end
