#!/bin/bash

cd ..
docker build --no-cache --tag comby/comby-base-dependencies-alpine-3.10 -f dockerfiles/alpine/base-dependencies/Dockerfile .
docker push comby/base-dependencies-alpine-3.10:latest
