#!/usr/bin/env bash

set -exuo pipefail

export PATH=$(pwd)/fake-path:$("$STACK_EXE" path --resolver ghc-9.2.3 --compiler-bin):$PATH
export STACK_ROOT=$(pwd)/fake-root

which ghc

"$STACK_EXE" --system-ghc --no-install-ghc --resolver ghc-9.2.3 ghc -- --info
"$STACK_EXE" --system-ghc --no-install-ghc --resolver ghc-9.2.3 runghc foo.hs
