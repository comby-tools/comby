#!/bin/bash

# mount /tmp/host to tmp in docker and run the binary
docker run -it -v /tmp/host:/tmp comby-alpine-binary-release
