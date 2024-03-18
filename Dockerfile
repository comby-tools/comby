##########################################################################################################################
### Binary build for testing. Pulled from registry. The base-dependencies-alpine spec builds this. Used by .travis.yml ###
##########################################################################################################################
FROM comby/comby:base-dependencies-alpine-3.19

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

RUN opam exec -- make build
RUN opam exec -- dune clean
# silence dune
RUN rm -rf comby
