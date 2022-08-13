#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver nightly-2022-08-02 async
eval `stack config env --resolver nightly-2022-08-02`
ghc Main.hs
