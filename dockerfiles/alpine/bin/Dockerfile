FROM alpine:3.9

RUN apk --no-cache add pcre-dev tini

COPY --from=comby-alpine-source-build /home/opam/comby/_build/default/src/main.exe /usr/local/bin/comby
COPY --from=comby-alpine-source-build /home/opam/comby/third-party-licenses /usr/local/bin/comby-third-party-licenses

WORKDIR /

ENTRYPOINT ["/sbin/tini", "--", "/usr/local/bin/comby"]
