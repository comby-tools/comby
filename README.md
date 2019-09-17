# comby

[![Apache-2.0](https://img.shields.io/badge/license-Apache-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/comby-tools/comby.svg?branch=master)](https://travis-ci.com/comby-tools/comby)
[![Coverage Status](https://coveralls.io/repos/github/comby-tools/comby/badge.svg?branch=master)](https://coveralls.io/github/comby-tools/comby?branch=master)
[![Downloads](https://img.shields.io/github/downloads/comby-tools/comby/total.svg?color=orange)](Downloads)
[![Commit](https://img.shields.io/github/last-commit/comby-tools/comby.svg)](Commit)
[![Gitter](https://img.shields.io/gitter/room/comby-tools/comby.svg?color=teal)](https://gitter.im/comby-tools/community)

![](https://user-images.githubusercontent.com/888624/64916761-0b657780-d752-11e9-96e2-cd81a2681139.gif)

## See the [usage documentation](https://comby.dev)

## Install

### Binary: `bash <(curl -sL get.comby.dev)`
### With docker: `docker pull comby/comby`

Running with docker on `stdin`:

```bash
echo '(👋 hi)' | docker run -a stdin -a stdout -i comby/comby '(:[emoji] hi)' 'bye :[emoji]' lisp -stdin
```

<img width="500" src="https://user-images.githubusercontent.com/888624/64924862-0edf1a00-d7b7-11e9-9c2e-cfeafde5bb4b.png">

### Or [try it live](https://bit.ly/2UXkonD).

## Build from source

- Install [opam](https://opam.ocaml.org/doc/Install.html)

- Create a new switch if you don't have OCaml installed:

```
opam init
opam switch create 4.07.0 4.07.0 
```

- [Install OS dependencies:](#os-dependencies)

  - **Linux:** `sudo apt-get install pkg-config libpcre3-dev`

  - **Mac:** `brew install pkg-config pcre`

- Then install the following opam libraries:

```
opam install -y \
ppx_deriving_yojson \
core \
ppxlib \
ppx_deriving \
angstrom \
hack_parallel \
opium \
pcre \
oasis \
tls \
camlzip \
bisect_ppx \
patdiff
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
