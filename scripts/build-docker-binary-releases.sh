#!/bin/bash

if [ -z "$1" ]; then
  echo "Set alpine version tag name, like 'alpine-3.14'"
fi

cd ..
docker build --tag comby-$1-binary-release -f dockerfiles/alpine/binary-release/Dockerfile .
docker build --tag comby-$1-binary-release-plus-rg -f dockerfiles/alpine/binary-release-plus-rg/Dockerfile .
docker build --tag comby-ubuntu-18.04-binary-release -f dockerfiles/ubuntu/binary-release/Dockerfile .
