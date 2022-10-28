#!/bin/sh
set -eu

stack run -- ls dependencies cabal > "$1"