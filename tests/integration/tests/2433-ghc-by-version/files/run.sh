#!/usr/bin/env bash

set -exuo pipefail

export PATH=$(pwd)/fake-path:$("$STACK_EXE" path --snapshot ghc-9.10.3 --compiler-bin):$PATH
export STACK_ROOT=$(pwd)/fake-root

which ghc

"$STACK_EXE" --system-ghc --no-install-ghc --snapshot ghc-9.10.3 ghc -- --info
"$STACK_EXE" --system-ghc --no-install-ghc --snapshot ghc-9.10.3 runghc test.hs
