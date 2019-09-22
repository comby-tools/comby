#!/bin/bash

# docker rmi comby/base-dependencies-alpine-3.10:X.X.X
cd ../dockerfiles/alpine/base-dependencies
docker build -t comby/base-dependencies-alpine-3.10:X.X.X .
# docker push comby/base-dependencies-alpine-3.10:latest
