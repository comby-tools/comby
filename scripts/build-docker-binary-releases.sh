#!/bin/bash

cd ..
docker build --tag comby-alpine-binary-release -f dockerfiles/alpine/binary-release/Dockerfile .
