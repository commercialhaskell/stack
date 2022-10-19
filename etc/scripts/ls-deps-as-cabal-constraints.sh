#!/bin/sh
#
# Lists the dependencies as exact (==) Cabal constraints for use with a Cabal
# `cabal.project` file, and pipes to the specified location (typically a file):
#
set -eu

stack ls dependencies cabal > "$1"
