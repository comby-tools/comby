open Core

module Omega = struct
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
end

module Alpha = struct
  open MParser

  (** Assumes the left and right delimiter are the same, and that these can be
      escaped. Does not parse a string body containing newlines (as usual when
      escaping with \n) *)
  module Escapable = struct
    module type S = sig
      val delimiter : string
      val escape : char
    end

    module Make (M : S) = struct
      (* delimiters can be escaped and parsing continues within the string body *)
      let escaped_char_s s =
        any_char s

      let char_token_s s =
        ((char M.escape >> escaped_char_s >>= fun c -> return (Format.sprintf {|%c%c|} M.escape c))
         <|> (any_char |>> String.of_char)
        )
          s

      let base_string_literal s =
        ((string M.delimiter >> (many_until char_token_s (string M.delimiter))
          |>> String.concat)
         >>= fun result ->
         return (Format.sprintf {|%s%s%s|} M.delimiter result M.delimiter)
        )
          s
    end
  end

  (** Quoted or raw strings. Allows different left and right delimiters, and
      disallows any sort of escaping. Does not support raw strings with identifiers
      yet, e.g., {blah|<string body>|blah} (OCaml) or delim`<string body>`delim
      syntax (Go) *)
  module Raw = struct
    module type S = sig
      val left_delimiter : string
      val right_delimiter : string
    end

    module Make (M : S) = struct
      let char_token_s s =
        (any_char_or_nl |>> String.of_char) s

      let base_string_literal s =
        ((
          string M.left_delimiter >> (many_until char_token_s (string M.right_delimiter))
          |>> String.concat <?> "raw string literal body")
         >>= fun result ->
         return (Format.sprintf {|%s%s%s|} M.left_delimiter result M.right_delimiter)
        )
          s
    end
  end
end
