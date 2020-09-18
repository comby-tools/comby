open Core
open Command.Let_syntax

open Configuration
open Command_configuration

let verbose_out_file = "/tmp/comby.out"

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

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

let list_supported_languages_and_exit omega =
  let (module Matcher : Matchers.Engine) =
    if omega then
      (module Matchers.Omega)
    else
      (module Matchers.Alpha)
  in
  let list =
    List.map Matcher.all ~f:(fun (module M) ->
        let ext = List.hd_exn M.extensions in
        Format.sprintf " -matcher %-10s%-10s\n" ext M.name)
    |> String.concat
  in
  Format.printf "%-20s%-10s@." "Option" "Language";
  Format.printf "%s%!" list;
  exit 0

let substitute_environment_only_and_exit anonymous_arguments json_environment =
  let rewrite_template =
    match anonymous_arguments with
    | Some { rewrite_template; _ } -> rewrite_template
    | None ->
      Format.eprintf
        "When the -substitute-only argument is active, a rewrite template must \
         be in the second anonymous argument. For example: `comby 'ignored' \
         'rewrite_template' -substitute-only 'JSON-for-the-environment'`.";
      exit 1
  in
  match Yojson.Safe.from_string (Option.value_exn json_environment) with
  | json ->
    begin
      Match.Environment.of_yojson json
      |> function
      | Ok environment ->
        let substituted, _ = Rewriter.Rewrite_template.substitute rewrite_template environment in
        Format.printf "%s@." substituted;
        exit 0
      | Error err ->
        Format.eprintf "Error, could not convert input to environment: %s@." err;
        exit 1
    end
  | exception Yojson.Json_error err ->
    Format.eprintf "Error, could not parse JSON to environment: %s@." err;
    exit 1

let base_command_parameters : (unit -> 'result) Command.Param.t =
  [%map_open
     (* flags. *)
    let sequential = flag "sequential" no_arg ~doc:"Run sequentially"
    and match_only = flag "match-only" no_arg ~aliases:["only-matching"; "o"] ~doc:"Only perform matching. Put an empty rewrite template as the second argument on the CLI (it is ignored)"
    and verbose = flag "verbose" no_arg ~doc:(Format.sprintf "Log to %s" verbose_out_file)
    and rule = flag "rule" (optional_with_default "where true" string) ~doc:"rule Apply rules to matches."
    and match_timeout = flag "timeout" (optional_with_default 3 int) ~doc:"seconds Set match timeout on a source. Default: 3 seconds"
    and target_directory = flag "directory" ~aliases:["d"] (optional_with_default (Sys.getcwd ()) string) ~doc:(Format.sprintf "path Run recursively on files in a directory relative to the root. Default is current directory: %s" @@ Sys.getcwd ())
    and directory_depth = flag "depth" (optional int) ~doc:"n Depth to recursively descend into directories"
    and templates = flag "templates" ~aliases:["config"; "configuration"] (optional (Arg_type.comma_separated string)) ~doc:"paths CSV of directories containing templates, or TOML configuration files"
    and file_filters = flag "extensions" ~aliases:["e"; "file-extensions"; "f"] (optional (Arg_type.comma_separated string)) ~doc:"extensions Comma-separated extensions to include, like \".go\" or \".c,.h\". It is just a file suffix, so you can use it to filter file names like \"main.go\". The extension will be used to infer a matcher, unless -custom-matcher or -matcher is specified"
    and override_matcher = flag "matcher" ~aliases:["m"; "lang"; "l"; "language"] (optional string) ~doc:"extension Use this matcher on all files regardless of their file extension, unless a -custom-matcher is specified"
    and custom_matcher = flag "custom-matcher" (optional string) ~doc:"path Path to a JSON file that contains a custom matcher"
    and zip_file = flag "zip" ~aliases:["z"] (optional string) ~doc:"zipfile A zip file containing files to rewrite"
    and json_lines = flag "json-lines" no_arg ~doc:"Output JSON line format"
    and json_only_diff = flag "json-only-diff" no_arg ~doc:"Output only the URI and diff in JSON line format"
    and overwrite_file_in_place = flag "in-place" no_arg ~doc:"Rewrite files on disk, in place"
    and number_of_workers = flag "jobs" (optional_with_default 4 int) ~doc:"n Number of worker processes. Default: 4"
    and dump_statistics = flag "statistics" ~aliases:["stats"] no_arg ~doc:"Dump statistics to stderr"
    and stdin = flag "stdin" no_arg ~doc:"Read source from stdin"
    and stdout = flag "stdout" no_arg ~doc:"Print changed content to stdout. Useful to editors for reading in changed content."
    and substitute_environment = flag "substitute-only" (optional string) ~doc:"JSON Substitute the environment specified in JSON into the rewrite template and output the substitution. Do not match or rewrite anything (match templates and inputs are ignored)."
    and diff = flag "diff" no_arg ~doc:"Output diff"
    and color = flag "color" no_arg ~doc:"Color matches or replacements (patience diff)."
    and newline_separated_rewrites = flag "newline-separated" no_arg ~doc:"Instead of rewriting in place, output rewrites separated by newlines."
    and count = flag "count" no_arg ~doc:"Display a count of matches in a file."
    and list = flag "list" no_arg ~doc:"Display supported languages and extensions"
    and exclude_directory_prefix = flag "exclude-dir" (optional_with_default ["."] (Arg_type.comma_separated ~strip_whitespace:true string)) ~doc:"prefixes Comma-separated prefixes of directories to exclude. Do not put whitespace between commas unless the string is quoted. Default: '.' (ignore directories starting with dot)"
    and exclude_file_prefix = flag "exclude" (optional_with_default []  (Arg_type.comma_separated ~strip_whitespace:true string)) ~doc:"prefixes Comma-separated prefixes of file names or file paths to exclude. Do not put whitespace between commas unless the string is quoted."
    and interactive_review = flag "review" ~aliases:["r"] no_arg ~doc:"Review each patch and accept, reject, or modify it with your editor of choice. Defaults to $EDITOR. If $EDITOR is unset, defaults to \"vim\". Override $EDITOR with the -editor flag."
    and editor = flag "editor" (optional string) ~doc:"editor Perform manual review with [editor]. This activates -review mode."
    and editor_default_is_reject = flag "default-no" no_arg ~doc:"If set, the default action in review (pressing return) will NOT apply the change. Setting this option activates -review mode."
    and disable_substring_matching = flag "disable-substring-matching" no_arg ~doc:"Allow :[holes] to match substrings"
    and omega = flag "omega" no_arg  ~doc:"Use Omega matcher engine."
    and fast_offset_conversion = flag "fast-offset-conversion" no_arg ~doc:"Enable fast offset conversion. This is experimental and will become the default once vetted."
    and match_newline_toplevel = flag "match-newline-at-toplevel" no_arg ~aliases:[] ~doc:"Enable matching newlines at the top level for :[hole]."
    and regex_pattern = flag "regex" no_arg ~doc:"print a regex that a file must satisfy in order for a pattern to be run"
    and ripgrep_args = flag "ripgrep" (optional string) ~aliases:["rg"] ~doc:"flags Activate ripgrep for filtering files. Add flags like '-g *.go' to include or exclude file extensions."
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
    let file_filters_to_paths file_filters =
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
    let anonymous_arguments =
      Option.map anonymous_arguments ~f:(fun (match_template, rewrite_template, file_filters) ->
          let file_filters = file_filters_to_paths file_filters in
          { match_template; rewrite_template; file_filters })
    in
    let file_filters =
      if Option.is_some zip_file then
        file_filters
      else
        Option.bind ~f:file_filters_to_paths file_filters
    in
    if list then list_supported_languages_and_exit omega;
    if Option.is_some substitute_environment then
      substitute_environment_only_and_exit anonymous_arguments substitute_environment;
    let interactive_review =
      let default_editor =
        let f = Option.some in
        let default = Some "vim" in
        let default = Option.value_map (Sys.getenv "EDITOR") ~default ~f in
        Option.value_map editor ~default ~f
      in
      let editor =
        if Option.is_some editor then
          editor
        else if interactive_review then
          default_editor
        else
          None
      in
      match editor with
      | Some editor ->
        Some { editor; default_is_accept = not editor_default_is_reject }
      | None when editor_default_is_reject ->
        Some { editor = (Option.value_exn default_editor)
             ; default_is_accept = false
             }
      | None -> None
    in
    let substitute_in_place = not newline_separated_rewrites in
    let omega_env = Option.is_some @@ Sys.getenv "OMEGA_COMBY" in
    let omega = omega || omega_env in
    let fast_offset_conversion_env = Option.is_some @@ Sys.getenv "FAST_OFFSET_CONVERSION_COMBY" in
    let fast_offset_conversion = fast_offset_conversion_env || fast_offset_conversion in
    let configuration =
      Command_configuration.create
        { input_options =
            { rule
            ; templates
            ; anonymous_arguments
            ; file_filters
            ; zip_file
            ; match_only
            ; stdin
            ; target_directory
            ; directory_depth
            ; exclude_directory_prefix
            ; exclude_file_prefix
            ; custom_matcher
            ; override_matcher
            ; regex_pattern
            ; ripgrep_args
            }
        ; run_options =
            { sequential
            ; verbose
            ; match_timeout
            ; number_of_workers
            ; dump_statistics
            ; substitute_in_place
            ; disable_substring_matching
            ; omega
            ; fast_offset_conversion
            ; match_newline_toplevel
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
    fun () ->
      Pipeline.run configuration;
      match configuration.extension with
      | Some ".generic" ->
        Format.eprintf "@.WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.@."
      | Some extension ->
        let (module M) = configuration.matcher in
        if String.equal M.name "Generic" then
          Format.eprintf "@.WARNING: the GENERIC matcher was used because I'm unable to guess what language to use for the file extension %s. The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.@." extension
        else if debug then Format.eprintf "@.NOTE: the %s matcher was inferred from extension %s. See '-list' to set a matcher for a specific language.@." M.name extension
      | None -> ()
  ]

let default_command =
  Command.basic ~summary:"Run a rewrite pass. Comby runs in current directory by default. The '-stdin' option rewrites input on stdin." base_command_parameters

let parse_comby_dot_file () =
  let open Toml in
  let open TomlTypes in
  match Parser.from_filename ".comby" with
  | `Error (s, _) -> Format.eprintf "TOML parse error in .comby file: %s@." s; exit 1
  | `Ok toml ->
    let flags = Table.find_opt (Toml.key "flags") toml in
    let to_flags = function
      | None -> []
      | Some TString s ->
        String.split_on_chars s ~on:[' '; '\t'; '\r'; '\n']
        |> List.filter ~f:(String.(<>) "")
      | Some v ->
        Format.eprintf "TOML value not a string: %s@." (Toml.Printer.string_of_value v);
        exit 1
    in
    to_flags flags

let () =
  Scheduler.Daemon.check_entry_point ();
  Command.run default_command ~version:"1.0.0" ~extend:(fun _ ->
      match Sys.file_exists ".comby" with
      | `Yes -> parse_comby_dot_file ()
      | _ -> [])
