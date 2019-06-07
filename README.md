# comby

[![Apache-2.0](https://img.shields.io/badge/license-Apache-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/comby-tools/comby.svg?branch=master)](https://travis-ci.com/comby-tools/comby)
[![Coverage Status](https://coveralls.io/repos/github/comby-tools/comby/badge.svg?branch=master)](https://coveralls.io/github/comby-tools/comby?branch=master)
[![Downloads](https://img.shields.io/github/downloads/comby-tools/comby/total.svg?color=orange)](Downloads)
[![Commit](https://img.shields.io/github/last-commit/comby-tools/comby.svg)](Commit)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](Gitter)

## `bash <(curl -sL get.comby.dev)`

### Or [try it live](https://comby.live/).

## Source Build

- Install [opam](https://opam.ocaml.org/doc/Install.html)

- Create a new switch if you don't have OCaml installed

```
opam init
opam switch create 4.05.0 4.05.0 
```

- Install dependencies

### Linux

```
sudo apt-get install pkg-config libpcre3-dev
```

### Mac

```
brew install pkg-config pcre
```

- Install opam libraries

```
opam install ppx_deriving_yojson
opam install core
opam install ppxlib
opam install ppx_deriving
opam install angstrom
opam install hack_parallel
opam install opium
opam install pcre
opam install oasis
opam install tls
opam install camlzip
```

- Install [mparser](https://github.com/comby-tools/mparser)

```
git clone https://github.com/comby-tools/mparser
oasis setup
ocaml setup.ml -configure --enable-pcre --enable-re
ocaml setup.ml -build
ocaml setup.ml -install
```

- Build and test

```
make
make test
```
