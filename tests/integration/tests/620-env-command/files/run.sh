#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.18 async
eval `stack config env --resolver lts-24.18`
ghc Main.hs
