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
docker build --platform linux/amd64 --tag comby-ubuntu-20.04-build . -f dockerfiles/ubuntu/binary-release/Dockerfile
docker run --platform linux/amd64 --rm --entrypoint cat comby-ubuntu-20.04-build:latest /home/comby/_build/default/src/main.exe > scripts/$VERSION/comby-$VERSION-x86_64-linux
cd scripts/$VERSION && tar czvf comby-$VERSION-x86_64-linux.tar.gz comby-$VERSION-x86_64-linux && cd ../..

# Build mac binary (deprecated--only rely on brew now)
# make clean
# make release
# make test

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
ALPINE_VERSION=alpine-3.19
cd scripts
./build-docker-binary-releases.sh $ALPINE_VERSION
docker tag comby-$ALPINE_VERSION-binary-release:latest comby/comby:$ALPINE_VERSION-$VERSION
docker tag comby-$ALPINE_VERSION-binary-release-plus-rg:latest comby/comby-rg:$ALPINE_VERSION-$VERSION
echo "test: docker run --platform linux/amd64 -it comby/comby:$ALPINE_VERSION-$VERSION -version"
echo "push: docker push comby/comby:$ALPINE_VERSION-$VERSION"
echo "tag latest (optional): docker tag comby/comby:$ALPINE_VERSION-$VERSION comby/comby:latest"
echo "push (optional): docker push comby/comby:latest"
echo
echo "rg sanity check"
echo "test: docker run --platform linux/amd64 --rm -it --entrypoint sh comby/comby-rg:$ALPINE_VERSION-$VERSION"
echo "test: cd /usr/local/bin/comby-third-party-licenses && comby 'a' 'b' -rg \"\" ALL.txt"
echo "push: docker push comby/comby-rg:$ALPINE_VERSION-$VERSION"
echo "tag latest (optional): docker tag comby/comby-rg:$ALPINE_VERSION-$VERSION comby/comby-rg:latest"
echo "push (optional): docker push comby/comby-rg:latest"
