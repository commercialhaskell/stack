#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.6 async
eval `stack config env --resolver lts-24.6`
ghc Main.hs
