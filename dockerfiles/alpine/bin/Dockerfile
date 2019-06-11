FROM alpine:3.9

RUN apk --no-cache add pcre-dev tini

COPY --from=comby-build-test /home/opam/comby/_build/default/src/main.exe /usr/local/bin/comby

WORKDIR /

ENTRYPOINT ["/sbin/tini", "--", "/usr/local/bin/comby"]
