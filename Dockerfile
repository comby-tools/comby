##################################################
### Source build and tests. Used by build.yaml ###
##################################################
FROM ocaml/opam:alpine-3.20-ocaml-5.3

WORKDIR /home/comby

RUN sudo apk --no-cache add \
    m4 \
    linux-headers \
    perl \
    pkgconf \
    gmp-dev \
    zlib-dev \
    zstd-dev \
    sqlite-dev \
    libev-dev \
    autoconf \
    openssl-dev

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
RUN opam exec -- make build
RUN opam exec -- dune clean
# silence dune
RUN rm -rf comby
