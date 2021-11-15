#!/bin/bash

opam exec -- bisect-ppx-report summary
echo '----'
opam exec -- bisect-ppx-report send-to Codecov
echo $?
