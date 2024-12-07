#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-22.43 async
eval `stack config env --resolver lts-22.43`
ghc Main.hs
