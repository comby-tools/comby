open Core

open Rewriter

open Test_helpers

open Test_alpha

let run_bash source match_template rewrite_template =
  Bash.first ~configuration match_template source
  |> function
  | Ok result ->
    Rewrite.all ~source ~rewrite_template [result]
    |> (fun x -> Option.value_exn x)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string
  | Error _ ->
    print_string rewrite_template

let run_go source match_template rewrite_template =
  Go.first ~configuration match_template source
  |> function
  | Ok result ->
    Rewrite.all ~source ~rewrite_template [result]
    |> (fun x -> Option.value_exn x)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string
  | Error _ ->
    print_string rewrite_template

let%expect_test "custom_long_delimiters" =
  let source =
    {|
      case
        case
          block 1
        esac

        case
          block 2
        esac
      esac
    |}
  in
  let match_template = {|case :[1] esac|} in
  let rewrite_template = {|case nuked blocks esac|} in

  run_bash source match_template rewrite_template;
  [%expect_exact {|
      case nuked blocks esac
    |}]

let%expect_test "custom_long_delimiters_doesn't_work_in_go" =
  let source =
    {|
      case
        case
          block 1
        esac

        case
          block 2
        esac
      esac
    |}
  in
  let match_template = {|case :[1] esac|} in
  let rewrite_template = {|case nuked blocks esac|} in

  run_go source match_template rewrite_template;
  [%expect_exact {|
      case nuked blocks esac

        case
          block 2
        esac
      esac
    |}]
