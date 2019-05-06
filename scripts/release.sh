#!/bin/bash

VERSION="0.x.0"

if [ -z "$1" ]
  then
    echo "Need arg: what version to release?"
fi

VERSION=$1

rm -rf $VERSION
mkdir -p $VERSION $VERSION/0 $VERSION/get

cd ../third-party-licenses
./pull-and-update-release-scripts.sh
cd ..

comby '"0.x.0"' "\"$VERSION\"" .ml -d src -i

make clean
make release
make test

git checkout -- .

OS=$(uname -s || echo dunno)

if [ "$OS" = "Darwin" ]; then
    cp _build/default/src/main.exe scripts/$VERSION/comby-$VERSION-x86_64-macos
    cd scripts/$VERSION && tar czvf comby-$VERSION-x86_64-macos.tar.gz comby-$VERSION-x86_64-macos && cd ../..
else
    cp _build/default/src/main.exe scripts/$VERSION/comby-$VERSION-x86_64-linux
    cd scripts/$VERSION && tar czvf comby-$VERSION-x86_64-linux.tar.gz comby-$VERSION-x86_64-linux && cd ../..
fi

cp scripts/check-and-install.sh scripts/$VERSION/0/index.html
cp scripts/install-with-licenses.sh scripts/$VERSION/get/index.html
comby '"0.x.0"' "$VERSION" * -i .html -d scripts/$VERSION
echo "Remember to binaries for linux."
