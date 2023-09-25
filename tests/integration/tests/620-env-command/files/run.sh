#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-21.13 async
eval `stack config env --resolver lts-21.13`
ghc Main.hs
