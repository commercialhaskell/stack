#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver nightly-2022-09-05 async
eval `stack config env --resolver nightly-2022-09-05`
ghc Main.hs
