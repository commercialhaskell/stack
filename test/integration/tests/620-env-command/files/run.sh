#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-20.26 async
eval `stack config env --resolver lts-20.26`
ghc Main.hs
