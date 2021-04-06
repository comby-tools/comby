open! Core
open! Import

include Patdiff_kernel.Html_output.Private.Make (struct
    let mtime file =
      let%map.Or_error stats =
        Or_error.try_with (fun () -> Unix.stat (File_name.real_name_exn file))
      in
      stats.st_mtime |> Time.Span.of_sec |> Time.of_span_since_epoch
    ;;
  end)
