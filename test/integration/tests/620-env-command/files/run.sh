#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-14.27 async
eval `stack config env --resolver lts-14.27`
ghc Main.hs
