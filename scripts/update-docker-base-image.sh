#!/bin/bash

cd ../dockerfiles/alpine/base-dependencies
docker build -t comby/base-dependencies-alpine:master .
docker push comby/base-dependencies-alpine:master
