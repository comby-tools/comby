Test handling of text files missing a trailing newline.

  $ echo "foo" > with_newline
  $ echo -n "foo" > missing_newline
  $ echo -ne "foo\n\n" > extra_newline

  $ patdiff -default with_newline with_newline

  $ patdiff -default missing_newline missing_newline
  No newline at the end of missing_newline
  No newline at the end of missing_newline

  $ patdiff -default missing_newline missing_newline -warn-if-no-trailing-newline false

  $ patdiff -default missing_newline with_newline
  No newline at the end of missing_newline

  $ patdiff -default missing_newline with_newline -warn-if-no-trailing-newline false
  No newline at the end of missing_newline

  $ patdiff -default missing_newline extra_newline | visible_colors
  No newline at the end of missing_newline
