#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-19.7 async
eval `stack config env --resolver lts-19.7`
ghc Main.hs
