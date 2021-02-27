open! Core

module Readme : sig
  val readme : string
end = struct
  let readme =
    {|.TH PATDIFF 1 "16 March 2011"
.SH NAME
patdiff \- find differences between two files

.SH SYNOPSIS
\fBpatdiff.exe\fP [options] \fIprev-file\fP \fInext-file\fP

.SH DESCRIPTION
Put simply, \fBpatdiff.exe\fP compares the contents of the two files \fIprev-file\fP and \fInext-file\fP.  If desired, a configuration file defining how to format the output can be used. The comparison is done using the patience diff algorithm

By default, compare two files and output the result of the comparison to the terminal, highlighting differences between the files on a word-by-word basis.  All whitespace is considered equal and effectively ignored.  The styling of the output is determined by the configuration file used.  A configuration file is searched for at ~/.patdiff, and if none is found, default styling is applied.  The exit code follows the diff standard, much like GNU diff. (0 = same, 1 = different, 2 = error)

.SS Options
Below is a summary of all of the options that \fIpatdiff.exe\fP accepts Options can be abbreviated to any unique prefix of their name. Multiple options cannot be combined into a single command line word; they must be separated by spaces.

\fB-file\fP \fIFILE\fP
Use FILE as configuration file (overrides ~/.patdiff)

\fB-default\fP
Use the default configuration file (overrides ~./patdiff)

\fB-context\fP \fINUM\fP
Display NUM lines of unchanged context before and after changes

\fB-unrefined\fP
Don't highlight differences between words, only highlight differences between lines (improves performance)

\fB-keep-whitespace\fP
Consider whitespace when comparing files

\fB-dont_produce_unified_lines\fP
When refining and highlighting the differences between the words of the files, don't unify the word difference into a single unified line.

\fB-quiet\fP
Only report whether files differ, don't print the actual differences.

\fB-shallow\fP
When comparing directories, don't recurse into subdirectories. Simply compare the files and directories in the immediate directories.  If two subdirectories have the same name but different contents, enabling this option will cause them to be reported as such but treated as the same.

\fB-double-check\fP
If files seem the same, double check with 'cmp'.  If they are not actually identical, report so, but still exit 0.

\fBmask-uniques\fP
When comparing directories, don't diff unique files against /dev/null.  This is useful if you know the contents of the unique files and don't need to see the contents of the entire file.

\fB-ext-cmp\fP \fIFILE\fP
By default, this program uses OCaml's String.compare function to compare each line.  With this option, you can provide an external program that takes two anonymous strings as arguments and returns exit codes according the diff standard (0 = same, 1 = different, 2 = error). \fIpatdiff.exe\fP will then use the external program when comparing lines.  Using this option implies -unrefined, because the external calls are expensive and refinement basically loses its meaning with an arbitrary compare	function.

\fBalt-old\fP \fINAME\fP
In the header of the output, use NAME instead of the default old filename and time last modified.

\fB-alt-new\fP \fINAME\fP
Same as -alt-old, but for the new filename.

\fB-make-config\fP \fIFILE\fP
Write the default configuration file to FILE. No other arguments (besides the configuration FILE) are required (or accepted) if this flag is used.

\fB-readme\fP
        Output a man page. No other arguments are required or accepted

\fBhelp\fP
        Output a brief summary of a subcommand, detailing its usage

.SH CONFIGURATION

Formatting for the output of patdiff is specified in the configuration file.  All fields are optional.  The following fields are available:

\fBconfig_path\fP Directs patdiff to load another configuration file instead of this one.  All other options in this file will be ignored.
        (config_path "/home/username/.patdiff2")

.SS Command Line Arguments
All command line arguments can be passed through the configuration file.  If an argument is specified in both the configuration file and the command line, the command line overrides the configuration file.

(context 3)
(unrefined true)
(word_unify true)
(keep_whitespace true)
(quiet true)
(shallow true)
(double_check true)
(mask_uniques true)
(alt_old "old")
(alt_new "new")
(ext_cmp "str_cmp.sh")

.SS Format and Styling
The following styles are available:

\fBBold Underline Emph\fP
(Emph is Underline in ANSI)

\fBDim Blink Inverse Hide\fP
Some terminals don't support these options well, if at all.

\fB(Fg color) (Foreground color) (Bg color) (Background color)\fP
The following colors are available for ANSI and HTML outputs:

\fBYellow Blue Black Red Green Magenta Cyan White Gray Default\fP
Bright_yellow  Bright_blue Bright_black Bright_red Bright_magenta Bright_cyan Bright_white Bright_green

Most formatting options consist of three fields: prefix, suffix, and
style.

The prefix and suffix fields have identical specifications:

  \fBtext\fP
  The characters that will be printed when the prefix or suffix
  is used.

  \fBstyle\fP
  The styling applied to the prefix or suffix text.

\fBstyle\fP
The styling applied to the contents of the line.


Line format options have one additional field: \fIword_same\fP

\fBword_same\fP
When refining this kind of line, the styling applied to words that are
the same between the two lines.

The following formatting options are available:

        line_same
        line_old
        line_new
        line_unified
        word_same_old
        word_same_new
        word_same_unified
        word_old
        word_new
        hunk
        header_old
        header_new

See the default configuration file to for a sample configuration.
|}
  ;;
end
