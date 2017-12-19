#!/bin/sh -e
#
# HLint binary release downloader.
#
# It downloads the latest HLint and unpacks to $PWD/hlint/.
# This is primarily used for our Travis CI setup.
#
# Please see https://github.com/ndmitchell/hlint#running-with-continuous-integration
# Ported from https://raw.githubusercontent.com/ndmitchell/neil/master/misc/travis.sh

PACKAGE=hlint
echo "Downloading $PACKAGE now ..."

RELEASES=$(curl --silent https://github.com/ndmitchell/$PACKAGE/releases)
URL=https://github.com/$(echo "$RELEASES" | grep -o '\"[^\"]*-x86_64-linux\.tar\.gz\"' | sed s/\"//g | head -n1)
VERSION=$(echo "$URL" | sed -e 's/.*-\([\.0-9]\+\)-x86_64-linux\.tar\.gz/\1/')

curl --progress-bar --location -o"$PACKAGE.tar.gz" "$URL"
tar -xzf "$PACKAGE.tar.gz" -C .
mv "$PACKAGE-$VERSION" "$PACKAGE"
