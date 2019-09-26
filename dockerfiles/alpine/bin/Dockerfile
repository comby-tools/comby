FROM alpine:3.10

RUN apk --no-cache add pcre tini

COPY --from=comby-alpine-source-build /home/opam/comby/_build/default/src/main.exe /usr/local/bin/comby
COPY --from=comby-alpine-source-build /home/opam/comby/docs/third-party-licenses /usr/local/bin/comby-third-party-licenses

WORKDIR /

ENTRYPOINT ["/sbin/tini", "--", "/usr/local/bin/comby"]
