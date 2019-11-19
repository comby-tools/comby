#!/bin/bash

cd ..
docker build --no-cache --tag comby/base-dependencies-alpine-3.10 -f dockerfiles/alpine/base-dependencies/Dockerfile .
docker push comby/base-dependencies-alpine-3.10:latest
echo "Now run ./build-docker-binary-releases.sh to make sure the base dependencies image is updated."
