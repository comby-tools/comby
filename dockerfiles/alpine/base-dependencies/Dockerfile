#########################
### Base Dependencies ###
#########################
FROM ocaml/opam:alpine-3.20-ocaml-5.2

WORKDIR /home/comby

# Install alpine system dependencies. Yes,these need to be -dev.
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

# Copy the source files.
COPY Makefile /home/comby/
COPY comby.opam /home/comby/
COPY comby-semantic.opam /home/comby/
COPY comby-kernel.opam /home/comby/
COPY dune-project /home/comby/dune-project
COPY dune /home/comby/
COPY docs /home/comby/docs
COPY src /home/comby/src
COPY lib /home/comby/lib
COPY test /home/comby/test
COPY push-coverage-report.sh /home/comby/

# Prepare for build.
RUN sudo chown -R $(whoami) /home/comby

# Build and install the OCaml dependencies.
RUN eval $(opam env) && opam repository set-url default https://opam.ocaml.org && opam update && opam install . --deps-only --with-test -y --no-checksums
# Delete the source files
RUN sudo rm -rf /home/comby
