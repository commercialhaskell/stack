#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-24.0 async
eval `stack config env --resolver lts-24.0`
ghc Main.hs
