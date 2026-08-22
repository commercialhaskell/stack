#!/usr/bin/env bash

set -euxo pipefail

eval `stack --snapshot lts-24.55 config env`
ghc Main.hs
