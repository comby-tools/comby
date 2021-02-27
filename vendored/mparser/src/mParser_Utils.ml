
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


module IO = struct

  let input chn buffer pos length =
    if pos < 0 || pos + length > Bytes.length buffer then
      invalid_arg "MParser_Utils.IO.input: invalid substring";
    let rec iter chars_read =
      let pos' = pos + chars_read in
      let length' = length - chars_read in
      let chars = Stdlib.input chn buffer pos' length' in
      if chars > 0 then
        iter (chars_read + chars)
      else
        chars_read
    in
    iter 0

end


module String = struct
  include String

  let unique =
    let module S = Set.Make (String) in
    fun l -> S.elements (List.fold_right S.add l S.empty)

  let for_all p a =
    let rec iter i =
      if i >= String.length a then
        true
      else if p (String.unsafe_get a i) then
        iter (i+1)
      else
        false
    in
    iter 0

end


module Bytes = struct
  include Bytes

  let match_sub b start pat =
    let len_b = Bytes.length b in
    let len_pat = String.length pat in
    if not (start >= 0 && start <= len_b) then
      invalid_arg "MParser_Utils.Bytes.match_sub: invalid index";
    if start + len_pat > len_b then
      false
    else
      let rec iter i =
        if i >= len_pat then
          true
        else if String.unsafe_get pat i <>
                Bytes.unsafe_get b (start + i) then
          false
        else
          iter (i+1)
      in
      iter 0

  let match_sub2 b1 i1 s2 i2 n =
    if not (i1 >= 0 && i1 + n <= Bytes.length b1 &&
            i2 >= 0 && i2 + n <= String.length s2) then
      invalid_arg "MParser_Utils.Bytes.match_sub2: invalid index";
    let rec iter i =
      if i >= n then
        true
      else if Bytes.unsafe_get b1 (i1 + i) <>
              String.unsafe_get s2 (i2 + i) then
        false
      else
        iter (i+1)
    in
    iter 0

end
