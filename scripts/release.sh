#!/bin/bash

VERSION="0.x.0"

if [ -z "$1" ]; then
    echo "Need arg: what version to release?"
    exit 1
fi

echo -n "Did you bump the number in main.ml?"
read X
echo -n "Did you commit release script changes?"
read X

VERSION=$1

rm -rf $VERSION
mkdir -p $VERSION $VERSION/0 $VERSION/get

cd ../docs/third-party-licenses
./pull-and-update-release-scripts.sh
cd ../..

comby '"0.x.0"' "\"$VERSION\"" .ml -d src -i

# Build ubuntu docker binary release and copy binary
docker build --tag comby-ubuntu-build . -f dockerfiles/ubuntu/binary-release/Dockerfile
docker run --rm --entrypoint cat comby-ubuntu-build:latest /home/comby/_build/default/src/main.exe > scripts/$VERSION/comby-$VERSION-x86_64-linux
cd scripts/$VERSION && tar czvf comby-$VERSION-x86_64-linux.tar.gz comby-$VERSION-x86_64-linux && cd ../..

# Build mac binary
make clean
make release
make test

git checkout -- .

OS=$(uname -s || echo dunno)

if [ "$OS" = "Darwin" ]; then
    cp _build/default/src/main.exe scripts/$VERSION/comby-$VERSION-x86_64-macos
    cd scripts/$VERSION && tar czvf comby-$VERSION-x86_64-macos.tar.gz comby-$VERSION-x86_64-macos && cd ../..
fi

cp scripts/check-and-install.sh scripts/$VERSION/0/index.html
cp scripts/install-with-licenses.sh scripts/$VERSION/get/index.html
comby '"0.x.0"' "$VERSION" .html -i -d scripts/$VERSION

# Alpine docker image
cd scripts
./build-docker-binary-releases.sh
docker tag comby-alpine-binary-release:latest comby/comby:$VERSION
echo "test: 'docker run -it comby/comby:$VERSION -version'"
echo "push: 'docker push comby/comby:$VERSION'"
echo "tag latest: 'docker tag comby/comby:$VERSION comby/comby:latest"
echo "push: 'docker push comby/comby:latest"
