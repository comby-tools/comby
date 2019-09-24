open Core
open Command.Let_syntax
open Hack_parallel

open Pipeline.Command_configuration

type json_result =
  { matches : Match.t list
  ; source : string
  }
[@@deriving yojson]

let verbose_out_file = "/tmp/comby.out"

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let select_matcher custom_matcher override_matcher configuration =
  if Option.is_some custom_matcher then
    let matcher_path = Option.value_exn custom_matcher in
    match Sys.file_exists matcher_path with
    | `No | `Unknown ->
      Format.eprintf "Could not open file: %s@." matcher_path;
      exit 1
    | `Yes ->
      Yojson.Safe.from_file matcher_path
      |> Matchers.Syntax.of_yojson
      |> function
      | Ok c -> Matchers.create c, None
      | Error error ->
        Format.eprintf "%s@." error;
        exit 1
  else if Option.is_some override_matcher then
    Matchers.select_with_extension (Option.value_exn override_matcher), None
  else
    let extension =
      match configuration.file_filters with
      | None | Some [] -> ".generic"
      | Some (filter::_) ->
        match Filename.split_extension filter with
        | _, Some extension -> "." ^ extension
        | extension, None -> "." ^ extension
    in
    Matchers.select_with_extension extension, Some extension

let paths_with_file_size paths =
  List.map paths ~f:(fun path ->
      let length =
        In_channel.create path
        |> fun channel ->
        In_channel.length channel
        |> Int64.to_int
        |> (fun value -> Option.value_exn value)
        |> (fun value -> In_channel.close channel; value)
      in
      (path, length))

let list_supported_languages_and_exit () =
  let list =
    List.map Matchers.all ~f:(fun (module M) ->
        let ext = List.hd_exn M.extensions in
        Format.sprintf " -matcher %-10s%-10s\n" ext M.name)
    |> String.concat
  in
  Format.printf "%-20s%-10s@." "Option" "Language";
  Format.printf "%s%!" list;
  exit 0

let base_command_parameters : (unit -> 'result) Command.Param.t =
  [%map_open
     (* flags. *)
    let sequential = flag "sequential" no_arg ~doc:"Run sequentially"
    and match_only = flag "match-only" no_arg ~doc:"Only perform matching. Put an empty rewrite template as the second argument on the CLI (it is ignored)"
    and verbose = flag "verbose" no_arg ~doc:(Format.sprintf "Log to %s" verbose_out_file)
    and rule = flag "rule" (optional_with_default "where true" string) ~doc:"rule Apply rules to matches."
    and match_timeout = flag "timeout" (optional_with_default 3 int) ~doc:"seconds Set match timeout on a source. Default: 3 seconds"
    and target_directory = flag "directory" ~aliases:["d"] (optional_with_default (Sys.getcwd ()) string) ~doc:(Format.sprintf "path Run recursively on files in a directory. Default is current directory: %s" @@ Sys.getcwd ())
    and directory_depth = flag "depth" (optional int) ~doc:"n Depth to recursively descend into directories"
    and specification_directories = flag "templates" (optional (Arg_type.comma_separated string)) ~doc:"path CSV of directories containing templates"
    and file_filters = flag "extensions" ~aliases:["e"; "file-extensions"; "f"] (optional (Arg_type.comma_separated string)) ~doc:"extensions Comma-separated extensions to include, like \".go\" or \".c,.h\". It is just a file suffix, so you can use it to filter file names like \"main.go\". The extension will be used to infer a matcher, unless -custom-matcher or -matcher is specified"
    and override_matcher = flag "matcher" ~aliases:["m"] (optional string) ~doc:"extension Use this matcher on all files regardless of their file extension, unless a -custom-matcher is specified"
    and custom_matcher = flag "custom-matcher" (optional string) ~doc:"path Path to a JSON file that contains a custom matcher"
    and zip_file = flag "zip" ~aliases:["z"] (optional string) ~doc:"zipfile A zip file containing files to rewrite"
    and json_lines = flag "json-lines" no_arg ~doc:"Output JSON line format"
    and json_only_diff = flag "json-only-diff" no_arg ~doc:"Output only the URI and diff in JSON line format"
    and overwrite_file_in_place = flag "in-place" no_arg ~doc:"Rewrite files on disk, in place"
    and number_of_workers = flag "jobs" (optional_with_default 4 int) ~doc:"n Number of worker processes. Default: 4"
    and dump_statistics = flag "statistics" ~aliases:["stats"] no_arg ~doc:"Dump statistics to stderr"
    and stdin = flag "stdin" no_arg ~doc:"Read source from stdin"
    and stdout = flag "stdout" no_arg ~doc:"Print changed content to stdout. Useful to editors for reading in changed content."
    and diff = flag "diff" no_arg ~doc:"Output diff"
    and color = flag "color" no_arg ~doc:"Color matches or replacements (patience diff)."
    and newline_separated_rewrites = flag "newline-separated" no_arg ~doc:"Instead of rewriting in place, output rewrites separated by newlines."
    and count = flag "count" no_arg ~doc:"Display a count of matches in a file."
    and list = flag "list" no_arg ~doc:"Display supported languages and extensions"
    and exclude_directory_prefix = flag "exclude-dir" (optional_with_default "." string) ~doc:"prefix of directories to exclude. Default: '.'"
    and interactive_review = flag "review" ~aliases:["r"] no_arg ~doc:"Review each patch and accept, reject, or modify it with your editor of choice. Defaults to $EDITOR. If $EDITOR is unset, defaults to \"vim\". Override $EDITOR with the -editor flag."
    and editor = flag "editor" (optional string) ~doc:"editor Perform manual review with [editor]. This activates -review mode."
    and anonymous_arguments =
      anon
        (maybe
           (t3
              ("MATCH_TEMPLATE" %: string)
              ("REWRITE_TEMPLATE" %: string)
              (sequence ("FULL_FILE_PATHS_OR_FILE_SUFFIXES" %: string))
           )
        )
    in
    let anonymous_arguments =
      Option.map anonymous_arguments ~f:(fun (match_template, rewrite_template, file_filters) ->
          let file_filters =
            match file_filters with
            | [] -> None
            | l ->
              List.map l ~f:(fun pattern ->
                  if String.contains pattern '/' then
                    match Filename.realpath pattern with
                    | exception Unix.Unix_error _ ->
                      Format.eprintf
                        "No such file or directory: %s. Comby interprets \
                         patterns containing '/' as file paths. If a pattern \
                         does not contain '/' (like '.ml'), it is considered a \
                         pattern where file endings must match the pattern. \
                         Please supply only valid file paths or patterns.@." pattern;
                      exit 1
                    | path -> path
                  else
                    pattern)
              |> Option.some
          in
          { match_template; rewrite_template; file_filters })
    in
    if list then list_supported_languages_and_exit ();
    let interactive_review =
      if Option.is_some editor then
        editor
      else if interactive_review then
        let f = Option.some in
        let default = Some "vim" in
        let default = Option.value_map (Sys.getenv "EDITOR") ~default ~f in
        Option.value_map editor ~default ~f
      else
        None
    in
    let substitute_in_place = not newline_separated_rewrites in
    let configuration =
      Pipeline.Command_configuration.create
        { input_options =
            { rule
            ; specification_directories
            ; anonymous_arguments
            ; file_filters
            ; zip_file
            ; match_only
            ; stdin
            ; target_directory
            ; directory_depth
            ; exclude_directory_prefix
            }
        ; run_options =
            { sequential
            ; verbose
            ; match_timeout
            ; number_of_workers
            ; dump_statistics
            ; substitute_in_place
            }
        ; output_options =
            { color
            ; count
            ; json_lines
            ; json_only_diff
            ; overwrite_file_in_place
            ; diff
            ; stdout
            ; substitute_in_place
            ; interactive_review
            }
        }
      |> function
      | Ok configuration -> configuration
      | Error error ->
        Format.eprintf "%s@." @@ Error.to_string_hum error;
        exit 1
    in
    let (module M) as matcher, extension = select_matcher custom_matcher override_matcher configuration in
    fun () ->
      Pipeline.run matcher configuration;
      match extension with
      | Some ".generic" ->
        Format.eprintf "@.WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.@."
      | Some extension ->
        if M.name = "Generic" then
          Format.eprintf "@.WARNING: the GENERIC matcher was used because I'm unable to guess what language to use for the file extension %s. The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.@." extension
        else if debug then Format.eprintf "@.NOTE: the %s matcher was inferred from extension %s. See '-list' to set a matcher for a specific language.@." M.name extension
      | None -> ()
  ]

let default_command =
  Command.basic ~summary:"Run a rewrite pass." base_command_parameters

let () =
  Scheduler.Daemon.check_entry_point ();
  Command.run default_command ~version:"0.8.0"
