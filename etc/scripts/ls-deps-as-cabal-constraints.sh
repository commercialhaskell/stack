#!/bin/sh
# Lists the dependencies as exact (==) Cabal constraints for use with a cabal
# project:
#  constraints:
#    , Cabal ==3.6.3.0
#    , Cabal-syntax ==3.6.0.0
#    , Glob ==0.10.2
set -eu

stack run -- ls dependencies cabal > "$1"