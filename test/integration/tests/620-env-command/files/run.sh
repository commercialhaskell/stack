#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-11.22 async
eval `stack config env --resolver lts-11.22`
ghc Main.hs
