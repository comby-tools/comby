## git version

- Extract a Js\_of\_ocaml-compatible library called `Patdiff_kernel`.
  `Patdiff_lib` is renamed to simply `Patdiff`.

- Fixed detection heuristic for binary files to function correctly given UTF-8
  input.

## v0.11

- Switch from `Core_extended.Std.Deprecated_command` to `Core.Command`.

## 113.24.00

- `patdiff -location-style omake` should print the line number of the
  first difference in each hunk, skipping context lines.

- Switched to PPX.

- Added binding in patdiff to use the newly minted colors of Ansi\_terminal.  This
  will be used notably by patdiff4 to produce better ddiff.

  Also, have the module `Color` and `Style` implement and export `Comparable.S`.
  This is useful for example to dedup styles from a list of styles without relying
  on the polymorphic equality.

- Make it so that if you pass `-warn-if-no-trailing-newline-in-both false`
  then you get the warning only when one file has a trailing newline and
  the other file does not.

  If you pass `-warn-if-no-trailing-newline-in-both true` or omit this
  flag, then you get the current behavior of warning for each file
  independently.

- Patdiff's unified-tests currently render colors codes in angle
  brackets.  Change them to square brackets.  Square brackets are word
  boundaries, so we'll get more legible diffs when tests fail.

- Simple code change in patdiff to prepare more changes in patdiff4.  This change
  is a pure refactoring and has zero runtime change.  Just moving some functions
  around.

- patdiff_core.ml is a very long module.  start extracting module from it.  start
  with format.  in the process, expose in a private fashion the record `Rule.t`.

- Continue on splitting the file patdiff_core.ml into smaller pieces.
  In this version, we extract each output mode into its own file.

- Kill the generation of html diffs in patdiff.  There are good third party tools
  that can convert efficiently ansi texts to html directly.
  We plan on simplifying a bit the patdiff source code to increase its
  maintainability, and dropping the requirement of producing html output seems a
  step in the right direction.

  Some pointers:

  http://www.pixelbeat.org/scripts/ansi2html.sh

## 112.24.00

Minor update: doc.

## 112.17.00

- The call to Pcre.full_split in patdiff_core.ml rely on a bug of
  pcre-ocaml <= 7.1.2.

  To get the same behavior with pcre-ocaml >= 7.1.3 we need to pass
  ~max:(-1).

  See this bug for more details:

     https://github.com/mmottl/pcre-ocaml/issues/1

## 111.25.00

- add a `?file_names` argument to `Compare_core.diff_strings`

## 111.21.00

- Added `Patdiff_core.iter_ansi`.

        (** Iter along the lines of the diff and the breaks between hunks. Offers more flexibility
            regarding what the caller wants to do with the lines *)
        val iter_ansi
          :  f_hunk_break:((int*int) -> (int*int) -> unit)
          -> f_line:(string -> unit)
          -> string Patience_diff.Hunk.t list
          -> unit

## 111.17.00

- Removed latex output.

## 109.53.00

- Bump version number
