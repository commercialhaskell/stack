#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-22.28 async
eval `stack config env --resolver lts-22.28`
ghc Main.hs
