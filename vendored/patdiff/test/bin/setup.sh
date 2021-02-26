#!/bin/bash

set -euo pipefail

HERE=$(readlink -f "$(dirname "${BASH_SOURCE[0]}")")
export HERE
PATH="$(readlink -f "$HERE/../../bin/"):$PATH"
export PATH

function patdiff {
    "$HERE/../../bin/main.exe" "$@"
}
export -f patdiff

function visible_colors {
    "$HERE/../../../ansicodes/bin/main.exe" visualize -minimize
}

export -f visible_colors

function start_test {
    set -u -o pipefail
}

export -f start_test
