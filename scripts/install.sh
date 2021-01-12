#!/bin/bash

set -ue
# set -x

RELEASE_VERSION="0.x.0"
RELEASE_TAG="0.x.0"
RELEASE_URL="https://github.com/comby-tools/comby/releases"

INSTALL_DIR=/usr/local/bin

function ctrl_c() {
        rm -f $TMP/$RELEASE_BIN &> /dev/null
        printf "${RED}[-]${NORMAL} Installation cancelled. Please see https://github.com/comby-tools/comby/releases if you prefer to install manually.\n"
        exit 1
}

trap ctrl_c INT

colors=0
if [ -z "$TERM" ] && which tput >/dev/null 2>&1; then
    colors=$(tput colors)
fi
if [ -t 1 ] && [ -n "$colors" ] && [ "$colors" -ge 8 ]; then
  RED="$(tput setaf 1)"
  GREEN="$(tput setaf 2)"
  YELLOW="$(tput setaf 3)"
  BOLD="$(tput bold)"
  NORMAL="$(tput sgr0)"
else
  RED=""
  GREEN=""
  YELLOW=""
  BOLD=""
  NORMAL=""
fi

EXISTS=$(command -v comby || echo)

if [ -n "$EXISTS" ]; then
    INSTALL_DIR=$(dirname $EXISTS)
fi

if [ ! -d "$INSTALL_DIR" ]; then
    printf "${YELLOW}[-]${NORMAL} $INSTALL_DIR does not exist. Please download the binary from ${RELEASE_URL} and install it manually.\n"
    exit 1
fi

TMP=${TMPDIR:-/tmp}

ARCH=$(uname -m || echo dunno)
case "$ARCH" in
    x86_64|amd64) ARCH="x86_64";;
#   x86|i?86) ARCH="i686";;
    *) ARCH="OTHER"
esac

OS=$(uname -s || echo dunno)

if [ "$OS" = "Darwin" ]; then
  OS=macos
fi

if [ "$OS" = "Linux" ]; then
  OS=linux
fi

RELEASE_BIN="comby-${RELEASE_TAG}-${ARCH}-${OS}"
RELEASE_TAR="$RELEASE_BIN.tar.gz"
RELEASE_URL="https://github.com/comby-tools/comby/releases/download/${RELEASE_TAG}/${RELEASE_TAR}"


if [ ! -e "$TMP/$RELEASE_TAR" ]; then
    printf "${GREEN}[+]${NORMAL} Downloading ${YELLOW}comby $RELEASE_VERSION${NORMAL}\n"

    SUCCESS=$(curl -s -L -o "$TMP/$RELEASE_TAR" "$RELEASE_URL" --write-out "%{http_code}")

    if [ "$SUCCESS" == "404" ]; then
        printf "${RED}[-]${NORMAL} No binary release available for your system.\n"
        rm -f $TMP/$RELEASE_TAR
        exit 1
    fi
	printf "${GREEN}[+]${NORMAL} Download complete.\n"
fi

cd "$TMP" && tar -xzf "$RELEASE_TAR"
chmod 755 "$TMP/$RELEASE_BIN"

echo "${GREEN}[+]${NORMAL} Installing comby to $INSTALL_DIR"
if [ ! $OS == "macos" ]; then
    sudo cp "$TMP/$RELEASE_BIN" "$INSTALL_DIR/comby"
else
    cp "$TMP/$RELEASE_BIN" "$INSTALL_DIR/comby"
fi

SUCCESS_IN_PATH=$(command -v comby || echo notinpath)

if [ $SUCCESS_IN_PATH == "notinpath" ]; then
    printf "${BOLD}[*]${NORMAL} Comby is not in your PATH. You should add $INSTALL_DIR to your PATH.\n"
    rm -f $TMP/$RELEASE_TAR
    rm -f $TMP/$RELEASE_BIN
    exit 1
fi


CHECK=$(printf 'printf("hello world!\\\n");' | $INSTALL_DIR/comby 'printf("hello :[1]!\\n");' 'printf("hello comby!\\n");' .c -stdin || echo broken)
if [ "$CHECK"  == "broken" ]; then
    printf "${RED}[-]${NORMAL} ${YELLOW}comby${NORMAL} did not install correctly.\n"
    printf "${YELLOW}[-]${NORMAL} My guess is that you need to install the pcre library on your system. Try:\n"
    if [ $OS == "macos" ]; then
        printf "${YELLOW}[*]${NORMAL} ${BOLD}brew install pcre && bash <(curl -sL get.comby.dev)${NORMAL}\n"
    else
        printf "${YELLOW}[*]${NORMAL} ${BOLD}sudo apt-get install libpcre3-dev && bash <(curl -sL get.comby.dev)${NORMAL}\n"
    fi
    rm -f $TMP/$RELEASE_BIN
    rm -f $TMP/$RELEASE_TAR
    exit 1
fi

rm -f $TMP/$RELEASE_BIN
rm -f $TMP/$RELEASE_TAR

printf "${GREEN}[+]${NORMAL} ${YELLOW}comby${NORMAL} is installed!\n"
printf "${GREEN}[+]${NORMAL} The licenses for this distribution are included in this script. Licenses are also available at https://github.com/comby-tools/comby/tree/master/docs/third-party-licenses\n"
printf "${GREEN}[+]${NORMAL} Running example command:\n"
echo "${YELLOW}------------------------------------------------------------"
printf "${YELLOW}comby${NORMAL} 'printf(\"${GREEN}:[1] :[2]${NORMAL}!\")' 'printf(\"comby, ${GREEN}:[1]${NORMAL}!\")' .c -stdin << EOF\n"
printf "int main(void) {\n"
printf "  printf(\"hello world!\");\n"
printf "}\n"
printf "EOF\n"
echo "${YELLOW}------------------------------------------------------------"
printf "${GREEN}[+]${NORMAL} Output:\n"
echo "${GREEN}------------------------------------------------------------${NORMAL}"
comby 'printf(":[1] :[2]!")' 'printf("comby, :[1]!")' .c -stdin << EOF
int main(void) {
  printf("hello world!");
}
EOF
echo "${GREEN}------------------------------------------------------------${NORMAL}"
