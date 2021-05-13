(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Angstrom.Unbuffered
open Core
open Async

let empty_bigstring = Bigstring.create 0

let rec finalize state result =
  (* It is very important to understand the assumptions that go into the second
   * case. If execution reaches that case, then that means that the parser has
   * commited all the way up to the last byte that was read by the reader, and
   * the reader's internal buffer is empty. If the parser hadn't committed up
   * to the last byte, then the reader buffer would not be empty and execution
   * would hit the first case rather than the second.
   *
   * In other words, the second case looks wrong but it's not. *)
  match state, result with
  | Partial p, `Eof_with_unconsumed_data s ->
    let bigstring = Bigstring.of_string s in
    finalize (p.continue bigstring ~off:0 ~len:(String.length s) Complete) `Eof
  | Partial p, `Eof                        ->
    finalize (p.continue empty_bigstring ~off:0 ~len:0 Complete) `Eof
  | Partial _, `Stopped () -> assert false
  | (Done _ | Fail _) , _  -> state_to_result state

let response = function
  | Partial p  -> `Consumed(p.committed, `Need_unknown)
  | Done(c, _) -> `Stop_consumed((), c)
  | Fail _     -> `Stop ()

let default_pushback () = Deferred.unit

let parse ?(pushback=default_pushback) p reader =
  let state = ref (parse p) in
  let handle_chunk buf ~pos ~len =
    begin match !state with
    | Partial p ->
      state := p.continue buf ~off:pos ~len Incomplete;
    | Done _ | Fail _ -> ()
    end;
    pushback () >>| fun () -> response !state
  in
  Reader.read_one_chunk_at_a_time reader ~handle_chunk >>| fun result ->
    finalize !state result

let async_many e k =
  Angstrom.(skip_many (e <* commit >>| k) <?> "async_many")

let parse_many p write reader =
  let wait = ref (default_pushback ()) in
  let k x = wait := write x in
  let pushback () = !wait in
  parse ~pushback (async_many p k) reader
