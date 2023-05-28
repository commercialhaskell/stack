#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-20.23 async
eval `stack config env --resolver lts-20.23`
ghc Main.hs
