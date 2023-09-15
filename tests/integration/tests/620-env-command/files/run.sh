#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-21.12 async
eval `stack config env --resolver lts-21.12`
ghc Main.hs
