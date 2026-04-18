#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.37 async
eval `stack config env --resolver lts-24.37`
ghc Main.hs
