#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-21.16 async
eval `stack config env --resolver lts-21.16`
ghc Main.hs
