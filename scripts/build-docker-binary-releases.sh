#!/bin/bash

cd ..
docker build --tag comby-alpine-binary-release -f dockerfiles/alpine/binary-release/Dockerfile .
docker build --tag comby-alpine-binary-release-plus-rg -f dockerfiles/alpine/binary-release-plus-rg/Dockerfile .
docker build --tag comby-ubuntu-binary-release -f dockerfiles/ubuntu/binary-release/Dockerfile .
