#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-17.10 async
eval `stack config env --resolver lts-17.10`
ghc Main.hs
