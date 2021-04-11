open Core

open Match

open Test_helpers

include Test_alpha

let %expect_test "statistics" =
  let template =
    {|
      def :[fn_name](:[fn_params])
    |}
    |> format
  in

  let source =
    {|
      def foo(bar):
        pass

      def bar(bazz):
        pass
    |}
    |> format
  in

  let rule =
    {| where true
    |}
    |> create
    |> Or_error.ok_exn
  in
  Go.all ~configuration ~template ~source ()
  |> List.filter ~f:(fun { environment; _ } ->
      Rule.(sat @@ apply rule environment))
  |> fun matches ->
  let statistics =
    Statistics.
      { number_of_files = 1
      ; lines_of_code = 5
      ; number_of_matches = List.length matches
      ; total_time = 0.0
      }
  in
  statistics
  |> Statistics.to_yojson
  |> Yojson.Safe.pretty_to_string
  |> print_string;
  [%expect {|
    {
      "number_of_files": 1,
      "lines_of_code": 5,
      "number_of_matches": 2,
      "total_time": 0.0
    } |}];

  let statistics' =
    Statistics.merge
      { number_of_files = 1
      ; lines_of_code = 10
      ; number_of_matches = 1
      ; total_time = 1.5
      }
      statistics
  in
  statistics'
  |> Statistics.to_yojson
  |> Yojson.Safe.pretty_to_string
  |> print_string;
  [%expect {|
    {
      "number_of_files": 2,
      "lines_of_code": 15,
      "number_of_matches": 3,
      "total_time": 1.5
    } |}]
