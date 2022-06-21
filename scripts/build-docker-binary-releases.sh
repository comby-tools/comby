#!/bin/bash

if [ -z "$1" ]; then
  echo "Set alpine version tag name, like 'alpine-3.14'"
  exit 1
fi

cd ..
docker build --platform linux/amd64 --tag comby-$1-binary-release -f dockerfiles/alpine/binary-release/Dockerfile .
docker build --platform linux/amd64 --tag comby-$1-binary-release-plus-rg -f dockerfiles/alpine/binary-release-plus-rg/Dockerfile .
docker build --platform linux/amd64 --tag comby-ubuntu-20.04-binary-release -f dockerfiles/ubuntu/binary-release/Dockerfile .
