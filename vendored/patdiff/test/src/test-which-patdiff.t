  $ start_test

Test that we are testing the patdiff in the tree, not /bin/patdiff.

  $ type patdiff
  patdiff is a function
  patdiff () 
  { 
      "$HERE/../../bin/main.exe" "$@"
  }
