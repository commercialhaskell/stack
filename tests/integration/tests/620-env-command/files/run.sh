#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.4 async
eval `stack config env --resolver lts-24.4`
ghc Main.hs
