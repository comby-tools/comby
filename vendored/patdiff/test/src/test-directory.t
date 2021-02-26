Test recursive diff of directories.

  $ mkdir prev
  $ mkdir prev/subdir
  $ mkdir prev/foo
  $ echo . > prev/foo/bar
  $ echo . > prev/this-goes-away
  $ echo prev > prev/this-changes
  $ echo prev > prev/subdir/this-changes-in-subdir

  $ mkdir next
  $ mkdir next/subdir
  $ echo . > next/foo
  $ echo . > next/this-appears
  $ echo next > next/this-changes
  $ echo next > next/subdir/this-changes-in-subdir

  $ patdiff -default prev next | visible_colors
  Only in prev: this-goes-away
  (fg:red)------ (+bold)prev/this-goes-away
  (fg:green)++++++ (+bold)/dev/null
  (fg:black)@|(+bold)-1,1 +1,0(off) ============================================================
  (fg:black bg:red)-|(fg:red).
  Only in next: this-appears
  (fg:red)------ (+bold)/dev/null
  (fg:green)++++++ (+bold)next/this-appears
  (fg:black)@|(+bold)-1,0 +1,1(off) ============================================================
  (fg:black bg:green)+|(fg:green).
  Files prev/foo and next/foo are not the same type
  (fg:red)------ (+bold)prev/subdir/this-changes-in-subdir
  (fg:green)++++++ (+bold)next/subdir/this-changes-in-subdir
  (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
  (fg:black bg:red)-|(fg:red)prev
  (fg:black bg:green)+|(fg:green)next
  (fg:red)------ (+bold)prev/this-changes
  (fg:green)++++++ (+bold)next/this-changes
  (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
  (fg:black bg:red)-|(fg:red)prev
  (fg:black bg:green)+|(fg:green)next

Test behavior of -alt-prev and -alt-next.

  $ patdiff -default prev next -alt-prev a -alt-next b | visible_colors
  Only in a: this-goes-away
  (fg:red)------ (+bold)a/this-goes-away
  (fg:green)++++++ (+bold)/dev/null
  (fg:black)@|(+bold)-1,1 +1,0(off) ============================================================
  (fg:black bg:red)-|(fg:red).
  Only in b: this-appears
  (fg:red)------ (+bold)/dev/null
  (fg:green)++++++ (+bold)b/this-appears
  (fg:black)@|(+bold)-1,0 +1,1(off) ============================================================
  (fg:black bg:green)+|(fg:green).
  Files a/foo and b/foo are not the same type
  (fg:red)------ (+bold)a/subdir/this-changes-in-subdir
  (fg:green)++++++ (+bold)b/subdir/this-changes-in-subdir
  (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
  (fg:black bg:red)-|(fg:red)prev
  (fg:black bg:green)+|(fg:green)next
  (fg:red)------ (+bold)a/this-changes
  (fg:green)++++++ (+bold)b/this-changes
  (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
  (fg:black bg:red)-|(fg:red)prev
  (fg:black bg:green)+|(fg:green)next
