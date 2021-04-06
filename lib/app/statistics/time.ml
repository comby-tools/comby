let start () = Unix.gettimeofday ()

let stop start =
  (Unix.gettimeofday () -. start) *. 1000.0

exception Time_out

let time_out ~after f args =
  let behavior =
    Sys.(signal sigalrm @@ Signal_handle (fun _ -> raise Time_out))
  in
  let cancel_alarm () =
    Unix.alarm 0 |> ignore;
    Sys.(set_signal sigalrm behavior)
  in
  Unix.alarm after |> ignore;
  match f args with
  | result ->
    cancel_alarm ();
    result
  | exception exc ->
    cancel_alarm ();
    raise exc
