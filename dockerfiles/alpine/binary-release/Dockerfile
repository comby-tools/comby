#######################################
### Source build for alpine release ###
#######################################
FROM ocaml/opam:alpine-3.20-ocaml-5.2 AS source-build-alpine

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

WORKDIR /home/comby

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
RUN opam exec -- dune runtest
RUN opam exec -- dune clean
# UGH. stop dune from complaining
RUN rm -rf comby
RUN opam exec -- dune build --profile release

###################################################
### Binary distribution on minimal alpine image ###
###################################################
FROM alpine:3.20

LABEL maintainer="Rijnard van Tonder <rvantonder@gmail.com>"
LABEL name="comby"
LABEL org.opencontainers.image.source="https://github.com/comby-tools/comby"

RUN apk --no-cache add libev sqlite-dev tini gmp openssl-dev
COPY --from=source-build-alpine /home/comby/_build/default/src/main.exe /usr/local/bin/comby
COPY --from=source-build-alpine /home/comby/docs/third-party-licenses /usr/local/bin/comby-third-party-licenses

WORKDIR /
ENTRYPOINT ["/sbin/tini", "--", "/usr/local/bin/comby"]
