#!/bin/bash

VERSION="0.x.0"

rm -rf $VERSION
mkdir $VERSION

cd ../third-party-licenses
./pull-and-update-release-scripts.sh
cd ..

make clean
make release
make test

cp _build/default/src/main.exe scripts/$VERSION/comby-$VERSION-x86_64-macos
cp scripts/check-and-install.sh scripts/$VERSION/0.sh
cp scripts/install-with-licenses.sh scripts/$VERSION/get.sh
comby '"0.x.0"' "$VERSION" * -i .sh -d scripts/$VERSION
echo "Remember to binaries for linux."
