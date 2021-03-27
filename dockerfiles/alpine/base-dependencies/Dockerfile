#########################
### Base Dependencies ###
#########################
FROM ocaml/opam2:alpine-3.12-ocaml-4.11

WORKDIR /home/comby

# Install alpine system dependencies.
RUN sudo apk --no-cache add \
    pcre-dev \
    m4 \
    linux-headers \
    perl \
    gmp-dev \
    zlib-dev \
    sqlite-dev \
    libev-dev \
    autoconf

# Copy the source files.
COPY Makefile /home/comby/
COPY comby.opam /home/comby/
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
RUN eval $(opam env) && opam repository set-url default https://opam.ocaml.org && opam update && opam install . --deps-only --with-test -y
# Delete the source files
RUN sudo rm -rf /home/comby
