#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-23.14 async
eval `stack config env --resolver lts-23.14`
ghc Main.hs
