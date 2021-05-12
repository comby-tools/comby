open Core

(* [run pattern args] accepts a [pattern] and list of extra ripgrep-compatible
   arguments [args], for example, ["-g"; "*.go"; "-g"; "*.ts"]. Returns a list
   of files if the commands succeeds. *)
val run : pattern:string -> args:string list -> string list option Or_error.t
