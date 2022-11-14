#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver nightly-2022-11-14 async
eval `stack config env --resolver nightly-2022-11-14`
ghc Main.hs
