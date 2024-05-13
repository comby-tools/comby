#!/bin/bash

cd ..
docker build --no-cache --tag comby/comby:base-dependencies-alpine-3.19 -f dockerfiles/alpine/base-dependencies/Dockerfile .
echo "If it succeeds, go and push the image:"
echo "docker push comby/comby:base-dependencies-alpine-3.19"
echo "Now run ./build-docker-binary-releases.sh to make sure the base dependencies image is updated."
