#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
RELEASE_SCRIPT=.local/bin/stack-release-script
rm -f "$RELEASE_SCRIPT"
(cd etc/scripts && stack --install-ghc build)
$(cd etc/scripts && stack exec which stack-release-script) --no-test-haddocks --arch=x86_64 --upload-label="Mac OS X 64-bit" release
