##################################################
### Source build and tests. Used by build.yaml ###
##################################################
FROM ocaml/opam:ubuntu-24.04-ocaml-5.3

WORKDIR /home/comby

RUN sudo apt-get update && sudo apt-get install -y \
    autoconf \
    pkg-config \
    zlib1g-dev \
    m4 \
    libgmp-dev \
    libev-dev \
    libsqlite3-dev \
    openssl \
    libssl-dev \
    libzstd-dev

COPY Makefile /home/comby/
COPY comby.opam /home/comby/
COPY comby-kernel.opam /home/comby/
COPY comby-semantic.opam /home/comby/
COPY dune-project /home/comby/
COPY dune /home/comby/
COPY docs /home/comby/docs
COPY src /home/comby/src
COPY lib /home/comby/lib
COPY test /home/comby/test
COPY push-coverage-report.sh /home/comby/

RUN sudo chown -R $(whoami) /home/comby

RUN opam repository set-url default https://opam.ocaml.org && opam update
RUN opam install . --deps-only --with-test -y
RUN opam install bisect_ppx -y
RUN opam exec -- make build
RUN opam exec -- dune clean
# silence dune
RUN rm -rf comby
