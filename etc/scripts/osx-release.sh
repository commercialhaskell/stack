#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
BUILD_DIR="$PWD"
cd "$(dirname "$0")/../.."
(cd etc/scripts && stack --install-ghc build)
RELEASE_SCRIPT="$(cd etc/scripts && stack exec which stack-release-script)"
cd "$BUILD_DIR"
"$RELEASE_SCRIPT" --no-test-haddocks --arch=x86_64 release
