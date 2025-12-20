#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.24 async
eval `stack config env --resolver lts-24.24`
ghc Main.hs
