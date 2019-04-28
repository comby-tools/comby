#!/bin/bash

set -ui

if which tput >/dev/null 2>&1; then
    colors=$(tput colors)
fi

if [ -t 1 ] && [ -n "$colors" ] && [ "$colors" -ge 8 ]; then
  YELLOW="$(tput setaf 3)"
  NORMAL="$(tput sgr0)"
else
  YELLOW=""
  NORMAL=""
fi

SUCCESS_IN_PATH=$(command -v comby || echo notinpath)

if [ $SUCCESS_IN_PATH == "notinpath" ]; then
    printf "${YELLOW}[-]${NORMAL} Looks like comby is not installed on your PATH. Let's try install it!\n"
    bash <(curl -sL get.comby.dev)
fi

exit 0

