#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.9 async
eval `stack config env --resolver lts-24.9`
ghc Main.hs
