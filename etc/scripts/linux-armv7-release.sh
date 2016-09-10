#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
(cd etc/scripts && stack --install-ghc build)
$(cd etc/scripts && stack exec which stack-release-script) --no-test-haddocks release
