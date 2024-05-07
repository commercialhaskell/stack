#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-22.21 async
eval `stack config env --resolver lts-22.21`
ghc Main.hs
