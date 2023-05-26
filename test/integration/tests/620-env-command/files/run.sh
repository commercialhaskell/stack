#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-20.22 async
eval `stack config env --resolver lts-20.22`
ghc Main.hs
