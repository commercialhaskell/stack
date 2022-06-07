#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver nightly-2022-06-06 async
eval `stack config env --resolver nightly-2022-06-06`
ghc Main.hs
