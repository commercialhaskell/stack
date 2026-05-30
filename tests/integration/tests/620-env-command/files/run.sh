#!/usr/bin/env bash

set -euxo pipefail

eval `stack --snapshot lts-24.43 config env`
ghc Main.hs
