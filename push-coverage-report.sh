#!/bin/bash

bisect-ppx-report send-to Coveralls
echo $?
bisect-ppx-report send-to Codecov
echo $?
