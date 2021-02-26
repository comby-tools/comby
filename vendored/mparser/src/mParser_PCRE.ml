
(* MParser, a simple monadic parser combinator library
   -----------------------------------------------------------------------------
   Copyright (C) 2008, Holger Arnold
                 2014-2020, Max Mouratov

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).
*)


module Regexp: MParser_Sig.Regexp = struct

  type t = Pcre.regexp
  type substrings = Pcre.substrings


  let compile_flags =
    Pcre.cflags [ `ANCHORED ]

  let make pattern =
    Pcre.regexp ~iflags:compile_flags pattern

  let get_substring s idx =
    try
      Some (Pcre.get_substring s idx)
    with Not_found | Invalid_argument _ ->
      None

  let get_all_substrings s =
    Pcre.get_substrings s

  let exec ~rex ~pos b =
    try
      Some (Pcre.exec ~pos ~rex (Bytes.unsafe_to_string b))
    with Not_found ->
      None

end

include MParser.MakeRegexp (Regexp)
