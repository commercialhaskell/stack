#!/usr/bin/env bash

set -exuo pipefail

export PATH=$(pwd)/fake-path:$("$STACK_EXE" path --resolver ghc-9.6.5 --compiler-bin):$PATH
export STACK_ROOT=$(pwd)/fake-root

which ghc

"$STACK_EXE" --system-ghc --no-install-ghc --resolver ghc-9.6.5 ghc -- --info
"$STACK_EXE" --system-ghc --no-install-ghc --resolver ghc-9.6.5 runghc foo.hs
