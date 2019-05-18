## basic OCaml and opam installation

full_apt_version () {
  package=$1
  version=$2
  case "${version}" in
      latest) echo -n "${package}" ;;
      *) echo -n "${package}="
         apt-cache show "$package" \
             | sed -n "s/^Version: \(${version}\)/\1/p" \
             | head -1
  esac
}

set -uex


# the ocaml version to test
OCAML_VERSION=${OCAML_VERSION:-latest}
SYS_OCAML_VERSION=4.02
# Default opam is the latest release of opam 2
OPAM_VERSION=${OPAM_VERSION:-2}
OPAM_INIT=${OPAM_INIT:-true}

OPAM_LATEST_RELEASE=2.0.3

case $OPAM_VERSION in
    2|2.0) OPAM_VERSION=$OPAM_LATEST_RELEASE;;
esac

if [ "$TRAVIS_OS_NAME" = "osx" ] ; then
    brew update &> /dev/null
    BREW_OPAM_VERSION=$(brew info opam --json=v1 | sed -e 's/.*"versions":{[^}]*"stable":"//' -e 's/".*//')
    if [ "$OPAM_VERSION" != "$BREW_OPAM_VERSION" ] ; then
        set +x
        echo -e "[\e[0;31mWARNING\e[0m] Ignored OPAM_VERSION=$OPAM_VERSION; interpreted as \"$BREW_OPAM_VERSION\"" >&2
        echo -e "[\e[0;31mWARNING\e[0m] opam 2 is installed via Homebrew" >&2
        set -x
    fi
    OPAM_VERSION="$BREW_OPAM_VERSION"
fi

if [ "$OPAM_VERSION" != "$OPAM_LATEST_RELEASE" ] ; then
    set +x
    echo -e "[\e[0;31mWARNING\e[0m] Out-of-date opam $OPAM_VERSION requested" >&2
    echo -e "[\e[0;31mWARNING\e[0m] Latest release is $OPAM_LATEST_RELEASE" >&2
    set -x
fi

if [ "${INSTALL_LOCAL+x}" = x ] ; then
  if [ "$TRAVIS_OS_NAME" = osx ] ; then
    echo INSTALL_LOCAL not permitted for macOS targets
    exit 1
  fi

  case ${OPAM_VERSION} in
      2*)
          if [ "${OPAM_SWITCH:=ocaml-system}" != ocaml-system ] ; then
              echo "INSTALL_LOCAL requires OPAM_SWITCH=ocaml-system (or unset/null)"
              exit 1
          fi ;;
      *)
          if [ "${OPAM_SWITCH:=system}" != system ] ; then
              echo "INSTALL_LOCAL requires OPAM_SWITCH=system (or unset/null)"
              exit 1
          fi ;;
  esac
fi

# the base opam repository to use for bootstrapping and catch-all namespace
case $OPAM_VERSION in
    2*) BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository} ;;
    *) BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository#1.2} ;;
esac

# whether we need a new gcc and binutils
UPDATE_GCC_BINUTILS=${UPDATE_GCC_BINUTILS:-"0"}

# Install Trusty remotes
UBUNTU_TRUSTY=${UBUNTU_TRUSTY:-"0"}

# Install XQuartz on OSX
INSTALL_XQUARTZ=${INSTALL_XQUARTZ:-"false"}

APT_UPDATED=0

add_ppa () {
    if [ "$TRAVIS_OS_NAME" = "linux" ] ; then
        APT_UPDATED=0
        sudo add-apt-repository --yes ppa:$1
    fi
}

apt_install () {
    if [ "$TRAVIS_OS_NAME" = "linux" ] ; then
        if [ "$APT_UPDATED" -eq 0 ] ; then
            APT_UPDATED=1
            sudo apt-get update -qq
        fi
        sudo apt-get install --no-install-recommends -y "$@"
        sudo apt-get install pkg-config libpcre3-dev
    fi
}

install_ocaml () {
    apt_install \
         ocaml ocaml-base ocaml-native-compilers ocaml-compiler-libs \
         ocaml-interp ocaml-base-nox ocaml-nox
}

install_opam2 () {
    case $TRAVIS_OS_NAME in
        linux)
            add_ppa ansible/bubblewrap
            if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
                install_ocaml
            fi
            apt_install bubblewrap
            sudo wget https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-linux -O /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
        osx)
            if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
                brew install ocaml
            fi
            sudo curl -fsSL https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-macos -o /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
    esac
}

install_ppa () {
  add_ppa $1
  if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
    sudo apt-get -qq update
    APT_UPDATED=1
    apt_install \
       "$(full_apt_version ocaml $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-base $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-native-compilers $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-compiler-libs $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-interp $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-base-nox $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-nox $SYS_OCAML_VERSION)"
  fi
  apt_install opam
}

install_on_linux () {
  case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12,1.2.2)
        OCAML_FULL_VERSION=3.12.1
        install_ppa avsm/ocaml42+opam12 ;;
    3.12,2*)
        OCAML_FULL_VERSION=3.12.1
        install_opam2 ;;
    4.00,1.2.2)
        OCAML_FULL_VERSION=4.00.1
        install_ppa avsm/ocaml42+opam12 ;;
    4.00,2*)
        OCAML_FULL_VERSION=4.00.1
        install_opam2 ;;
    4.01,1.2.2)
        OCAML_FULL_VERSION=4.01.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.01,2*)
        OCAML_FULL_VERSION=4.01.0
        install_opam2 ;;
    4.02,1.1.2)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam11 ;;
    4.02,1.2.0)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam120 ;;
    4.02,1.2.1)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam121 ;;
    4.02,1.2.2)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam12 ;;
    4.02,2*)
        OCAML_FULL_VERSION=4.02.3
        install_opam2 ;;
    4.03,1.2.2)
        OCAML_FULL_VERSION=4.03.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.03,2*)
        OCAML_FULL_VERSION=4.03.0
        install_opam2 ;;
    4.04,1.2.2)
        OCAML_FULL_VERSION=4.04.2
        install_ppa avsm/ocaml42+opam12 ;;
    4.04,2*)
        OCAML_FULL_VERSION=4.04.2
        install_opam2 ;;
    4.05,1.2.2)
        OCAML_FULL_VERSION=4.05.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.05,2*)
        OCAML_FULL_VERSION=4.05.0
        install_opam2 ;;
    4.06,1.2.2)
        OCAML_FULL_VERSION=4.06.1
        install_ppa avsm/ocaml42+opam12 ;;
    4.06,2*)
        OCAML_FULL_VERSION=4.06.1
        install_opam2 ;;
    4.07,1.2.2)
        OCAML_FULL_VERSION=4.07.1
        install_ppa avsm/ocaml42+opam12 ;;
    4.07,2*)
        OCAML_FULL_VERSION=4.07.1
        install_opam2 ;;
    *) echo "Unknown OCAML_VERSION=$OCAML_VERSION OPAM_VERSION=$OPAM_VERSION"
       echo "(An unset OCAML_VERSION used to default to \"latest\", but you must now specify it."
       echo "Try something like \"OCAML_VERSION=3.12\", \"OCAML_VERSION=4.07\", or see README-travis.md at https://github.com/ocaml/ocaml-ci-scripts )"
       exit 1 ;;
  esac

  TRUSTY="deb mirror://mirrors.ubuntu.com/mirrors.txt trusty main restricted universe"

  if [ "$UPDATE_GCC_BINUTILS" != "0" ] ; then
    echo "installing a recent gcc and binutils (mainly to get mirage-entropy-xen working!)"
    sudo add-apt-repository "${TRUSTY}"
    sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
    sudo apt-get -qq update
    sudo apt-get install -y gcc-4.8
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 90
    sudo add-apt-repository -r "${TRUSTY}"
  fi

  if [ "$UBUNTU_TRUSTY" != "0" ] ; then
    echo "Adding Ubuntu Trusty mirrors"
    sudo add-apt-repository "${TRUSTY}"
    sudo apt-get -qq update
    APT_UPDATED=1
  fi

  if [ "${INSTALL_LOCAL:=0}" != 0 ] ; then
    echo -en "travis_fold:start:build.ocaml\r"
    echo "Building a local OCaml; this may take a few minutes..."
    wget "http://caml.inria.fr/pub/distrib/ocaml-${OCAML_FULL_VERSION%.*}/ocaml-$OCAML_FULL_VERSION.tar.gz"
    tar -xzf "ocaml-$OCAML_FULL_VERSION.tar.gz"
    cd "ocaml-$OCAML_FULL_VERSION"
    ./configure -prefix /usr/local ${OCAML_CONFIGURE_ARGS:=--with-debug-runtime}
    make world.opt
    sudo make install
    cd ..
    echo -en "travis_fold:end:build.ocaml\r"
  fi
}

install_on_osx () {
  case $INSTALL_XQUARTZ in
      true)
        curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
        sudo hdiutil attach XQuartz-2.7.6.dmg
        sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
        ;;
  esac
  brew upgrade python || true
  brew install pkg-config || true
  brew install pcre || true
  case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12,1.2.2) OCAML_FULL_VERSION=3.12.1; brew install opam ;;
    3.12,2*) OCAML_FULL_VERSION=3.12.1; install_opam2 ;;
    4.00,1.2.2) OCAML_FULL_VERSION=4.00.1; brew install opam ;;
    4.00,2*) OCAML_FULL_VERSION=4.00.1; install_opam2 ;;
    4.01,1.2.2) OCAML_FULL_VERSION=4.01.0; brew install opam ;;
    4.01,2*) OCAML_FULL_VERSION=4.01.0; install_opam2 ;;
    4.02,1.2.2) OCAML_FULL_VERSION=4.02.3; brew install opam ;;
    4.02,2*) OCAML_FULL_VERSION=4.02.3; install_opam2 ;;
    4.03,1.2.2) OCAML_FULL_VERSION=4.03.0; brew install opam ;;
    4.03,2*) OCAML_FULL_VERSION=4.03.0; install_opam2 ;;
    4.04,1.2.2) OCAML_FULL_VERSION=4.04.2; brew install opam ;;
    4.04,2*) OCAML_FULL_VERSION=4.04.2; install_opam2 ;;
    4.05,1.2.2) OCAML_FULL_VERSION=4.05.0; brew install opam ;;
    4.05,2*) OCAML_FULL_VERSION=4.05.0; install_opam2 ;;
    4.06,1.2.2) OCAML_FULL_VERSION=4.06.1; brew install opam ;;
    4.06,2*) OCAML_FULL_VERSION=4.06.1; install_opam2 ;;
    4.07,1.2.2) OCAML_FULL_VERSION=4.07.1;
                OPAM_SWITCH=${OPAM_SWITCH:-system};
                brew install ocaml;
                brew install opam ;;
    4.07,2*) OCAML_FULL_VERSION=4.07.1;
                OPAM_SWITCH=${OPAM_SWITCH:-ocaml-system};
                brew install ocaml;
                install_opam2 ;;
    *) echo "Unknown OCAML_VERSION=$OCAML_VERSION OPAM_VERSION=$OPAM_VERSION"
       exit 1 ;;
  esac
}

case $TRAVIS_OS_NAME in
    osx) install_on_osx ;;
    linux) install_on_linux ;;
esac

OPAM_SWITCH=${OPAM_SWITCH:-ocaml-base-compiler.$OCAML_FULL_VERSION}

export OPAMYES=1

case $OPAM_INIT in
  true)
      opam init -a "$BASE_REMOTE" --comp="$OPAM_SWITCH"
      eval $(opam config env)
      ;;
esac

echo OCAML_VERSION=$OCAML_VERSION >  .travis-ocaml.env
echo OPAM_SWITCH=$OPAM_SWITCH     >> .travis-ocaml.env

if [ -x "$(command -v ocaml)" ]; then
    ocaml -version
else
    echo "OCaml is not yet installed"
fi

opam --version
opam --git-version

opam install ppx_deriving_yojson
opam install core
opam install ppxlib
opam install ppx_deriving
opam install tls
opam install angstrom
opam install hack_parallel
opam install opium
opam install pcre
opam install oasis
opam install camlzip
opam install bisect_ppx
opam install patdiff

git clone https://github.com/comby-tools/mparser
cd mparser
oasis setup
ocaml setup.ml -configure --enable-pcre --enable-re
ocaml setup.ml -build
ocaml setup.ml -install
cd -

make
make test

echo $TRAVIS_JOB_ID
bisect-ppx-report \
    -I _build/default/ \
    -coveralls coverage.json \
    -service-name travis-ci \
    -service-job-id $TRAVIS_JOB_ID \
    `find . -name 'bisect*.out'`
curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
