#!/bin/bash

bisect-ppx-report send-to Coveralls
echo $?
echo '---'
bisect-ppx-report summary
echo '----'
bisect-ppx-report send-to Codecov
echo $?
