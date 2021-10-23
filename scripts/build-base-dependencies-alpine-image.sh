#!/bin/bash

cd ..
docker build --no-cache --tag comby/base-dependencies-alpine-3.14 -f dockerfiles/alpine/base-dependencies/Dockerfile .
echo "If it succeeds, go and push the image:"
echo "docker push comby/base-dependencies-alpine-3.14:latest"
echo "Now run ./build-docker-binary-releases.sh to make sure the base dependencies image is updated."
