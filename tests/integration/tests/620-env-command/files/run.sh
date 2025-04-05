#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-23.17 async
eval `stack config env --resolver lts-23.17`
ghc Main.hs
