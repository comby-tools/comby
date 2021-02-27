open! Core
open! Async

include module type of struct
  include Expect_test_helpers_core
  include Expect_test_helpers_async
end

val links : (string * [ `In_path_as | `In_temp_as ] * string) list
val patdiff : extra_flags:string list -> prev:string -> next:string -> unit Deferred.t

val patdiff_dir
  :  extra_flags:string list
  -> prev:(Filename.t * string) list
  -> next:(Filename.t * string) list
  -> unit Deferred.t
