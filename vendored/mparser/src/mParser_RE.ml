
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

  type t = Re.re
  type substrings = Re.Group.t


  let compile_flags =
    [ `Anchored ]

  let make pattern =
    Re.Perl.(compile (re ~opts:compile_flags pattern))

  let get_substring s idx =
    try
      Some (Re.Group.get s idx)
    with Not_found ->
      None

  let get_all_substrings s =
    Re.Group.all s

  let exec ~rex ~pos b =
    try
      Some (Re.exec ~pos rex (Bytes.unsafe_to_string b))
    with Not_found ->
      None

end

include MParser.MakeRegexp (Regexp)
