FROM comby/base-dependencies-alpine-3.10:latest

WORKDIR /tmp/comby

COPY Makefile /tmp/comby/
COPY comby.opam /tmp/comby/
COPY dune /tmp/comby/
COPY docs /tmp/comby/docs
COPY src /tmp/comby/src
COPY lib /tmp/comby/lib
COPY test /tmp/comby/test
COPY push-coverage-report.sh /tmp/comby/

RUN sudo chown -R $(whoami) .
