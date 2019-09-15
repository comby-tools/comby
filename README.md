# comby

[![Apache-2.0](https://img.shields.io/badge/license-Apache-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/comby-tools/comby.svg?branch=master)](https://travis-ci.com/comby-tools/comby)
[![Coverage Status](https://coveralls.io/repos/github/comby-tools/comby/badge.svg?branch=master)](https://coveralls.io/github/comby-tools/comby?branch=master)
[![Downloads](https://img.shields.io/github/downloads/comby-tools/comby/total.svg?color=orange)](Downloads)
[![Commit](https://img.shields.io/github/last-commit/comby-tools/comby.svg)](Commit)
[![Gitter](https://img.shields.io/gitter/room/comby-tools/comby.svg?color=teal)](https://gitter.im/comby-tools/community)

![high-quality-bigger-crop-lets-go-final](https://user-images.githubusercontent.com/888624/64916761-0b657780-d752-11e9-96e2-cd81a2681139.gif)

## See the [usage documentation](https://comby.dev)

## Install

### Binary: `bash <(curl -sL get.comby.dev)`
### With docker: `docker pull comby/comby`

Running with docker on `stdin`:

```bash
echo '(👋 hi)' | docker run -a stdin -a stdout -i comby/comby '(:[emoji] hi)' 'bye :[emoji]' lisp -stdin
```

<img width="500" alt="Screen Shot 2019-09-15 at 12 31 07 PM" src="https://user-images.githubusercontent.com/888624/64924630-c1fa4400-d7b4-11e9-8b6c-b2d357be6a2b.png">

### Or [try it live](https://comby.live/).

## Build from source

- Install [opam](https://opam.ocaml.org/doc/Install.html)

- Create a new switch if you don't have OCaml installed:

```
opam init
opam switch create 4.07.0 4.07.0 
```

- Install OS dependencies:

  - **Linux:** `sudo apt-get install pkg-config libpcre3-dev`

  - **Mac:** `brew install pkg-config pcre`

- Then install the following opam libraries:

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
opam install bisect_ppx 
opam install patdiff
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
