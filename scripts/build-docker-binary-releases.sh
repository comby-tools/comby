#!/bin/bash

cd ..
docker build --tag comby-aline-binary-release -f dockerfiles/alpine/binary-release/Dockerfile .
