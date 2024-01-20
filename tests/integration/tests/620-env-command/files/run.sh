#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-22.6 async
eval `stack config env --resolver lts-22.6`
ghc Main.hs
