(lang dune 1.11)

(license "LGPL-2.1 with OCaml linking exception")
(name mparser-comby)

(authors "Holger Arnold" "Max Mouratov" "Rijnard van Tonder")
(maintainers "Rijnard van Tonder")
(source (github comby/mparser))

(package
 (name mparser-comby)
 (synopsis "A simple monadic parser combinator library")
 (description "
This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen and
the FParsec library for FSharp by Stephan Tolksdorf.")
 (depends
  (dune (>= 1.11))))

(package
 (name mparser-comby-pcre)
 (synopsis "MParser plugin: PCRE-based regular expressions")
 (depends
  mparser-comby
  pcre
  (dune (>= 1.11))))

(package
 (name mparser-comby-re)
 (synopsis "MParser plugin: RE-based regular expressions")
 (depends
  mparser-comby
  (re (>= 1.5))
  (dune (>= 1.11))))

(generate_opam_files true)
