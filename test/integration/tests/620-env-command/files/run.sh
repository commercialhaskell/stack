#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-21.8 async
eval `stack config env --resolver lts-21.8`
ghc Main.hs
