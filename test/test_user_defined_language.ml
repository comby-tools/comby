open Core
open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run (module M : Matchers.Matcher) source match_template rewrite_template =
  M.first ~configuration match_template source
  |> function
  | Ok result ->
    Rewrite.all ~source ~rewrite_template [result]
    |> (fun x -> Option.value_exn x)
    |> (fun {rewritten_source; _} -> rewritten_source)
    |> print_string
  | Error _ ->
    print_string rewrite_template

let%expect_test "user_defined_language" =
  let c =
    Syntax.
      { user_defined_delimiters = [("case", "esac")]
      ; escapable_string_literals = None
      ; raw_string_literals = []
      ; comments = [Multiline ("/*", "*/"); Until_newline "//"]
      }
  in
  let user_lang = Matchers.create c in
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
      /*
      case
        ignore this
      esac
      */
      // case
      //   ignore this
      // esac
    |}
  in
  let match_template = {|case :[1] esac|} in
  let rewrite_template = {|case nuked blocks esac|} in
  run user_lang source match_template rewrite_template ;
  [%expect_exact {|
      case nuked blocks esac
      /*
      case
        ignore this
      esac
      */
      // case
      //   ignore this
      // esac
    |}]

let%expect_test "user_defined_language_from_json" =
  let json =
    {|{
      "user_defined_delimiters": [
        ["case", "esac"]
      ],
      "escapable_string_literals": {
        "delimiters": ["\""],
        "escape_character": "\\"
      },
      "raw_string_literals": [],
      "comments": [
        [ "Multiline", "/*", "*/" ],
        [ "Until_newline", "//" ]
      ]
      }
    |}
  in
  let user_lang =
    Yojson.Safe.from_string json
    |> Matchers.Syntax.of_yojson
    |> Result.ok_or_failwith
    |> Matchers.create
  in
  let source = "" in
  let match_template = {|""|} in
  let rewrite_template = {|""|} in
  run user_lang source match_template rewrite_template ;
  [%expect_exact {|""|}]

let%expect_test "user_defined_language_from_json_optional_escapable" =
  let json =
    {|{
      "user_defined_delimiters": [
        ["case", "esac"]
      ],
      "raw_string_literals": [],
      "comments": [
        [ "Multiline", "/*", "*/" ],
        [ "Until_newline", "//" ]
      ]
      }
    |}
  in
  let user_lang =
    Yojson.Safe.from_string json
    |> Matchers.Syntax.of_yojson
    |> Result.ok_or_failwith
    |> Matchers.create
  in
  let source = "" in
  let match_template = {|""|} in
  let rewrite_template = {|""|} in
  run user_lang source match_template rewrite_template ;
  [%expect_exact {|""|}]
