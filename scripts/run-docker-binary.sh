#!/bin/bash

# mount /tmp/host to tmp in docker and run the binary
docker run -it -v /tmp/host:/tmp comby-alpine-3.19-binary-release -version
docker run -it -v /tmp/host:/tmp comby-alpine-3.19-binary-release-plus-rg -version
echo 'For ubuntu, do: docker run -it -v /tmp/host:/tmp comby-ubuntu-20.04-binary-release'
echo 'Then run:       ./_build/default/src/main.exe -version'
