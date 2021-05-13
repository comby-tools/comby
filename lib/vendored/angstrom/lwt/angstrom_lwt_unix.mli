(*---------------------------------------------------------------------------
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

open Angstrom


val parse
  : ?pushback:(unit -> unit Lwt.t)
  -> 'a t
  -> Lwt_io.input_channel
  -> (Buffered.unconsumed * ('a, string) result) Lwt.t

val parse_many
  : 'a t
  -> ('a -> unit Lwt.t)
  -> Lwt_io.input_channel
  -> (Buffered.unconsumed * (unit, string) result) Lwt.t

(** Useful for resuming a {!parse} that returns unconsumed data. Construct a
    [Buffered.state] by using [Buffered.parse] and provide it into this
    function. This is essentially what {!parse_many} does, so consider using
    that if you don't require fine-grained control over how many times you want
    the parser to succeed.

    Usage example:

    {[
      parse parser in_channel >>= fun (unconsumed, result) ->
      match result with
      | Ok a ->
        let { buf; off; len } = unconsumed in
        let state = Buffered.parse parser in
        let state = Buffered.feed state (`Bigstring (Bigstringaf.sub ~off ~len buf)) in
        with_buffered_parse_state state in_channel
      | Error err -> failwith err
    ]} *)
val with_buffered_parse_state
  : ?pushback:(unit -> unit Lwt.t)
  -> 'a Buffered.state
  -> Lwt_io.input_channel
  -> (Buffered.unconsumed * ('a, string) result) Lwt.t

