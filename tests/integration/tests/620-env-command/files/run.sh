#!/usr/bin/env bash

set -euxo pipefail

stack build --resolver lts-21.11 async
eval `stack config env --resolver lts-21.11`
ghc Main.hs
