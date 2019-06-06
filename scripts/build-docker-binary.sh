#!/bin/bash

cd ../dockerfiles/alpine
# build from source using alpine image
docker build -f src/Dockerfile -t comby-alpine-source-build $(mktemp -d)
# build a small binary image only (copies the binary build in the above to minimal alpine image)
docker build -f bin/Dockerfile -t comby-alpine-binary-build .
