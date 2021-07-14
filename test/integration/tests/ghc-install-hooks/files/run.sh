#!/usr/bin/env bash

set -exuo pipefail

stack_bin=$("$STACK_EXE" path --resolver ghc-8.6.5 --compiler-bin)

export STACK_ROOT=$(pwd)/fake-root

mkdir -p "${STACK_ROOT}"/hooks

echo "echo ${stack_bin}/ghc" > "${STACK_ROOT}"/hooks/ghc-install.sh
chmod +x "${STACK_ROOT}"/hooks/ghc-install.sh

"$STACK_EXE" --no-install-ghc --resolver ghc-8.6.5 ghc -- --info
"$STACK_EXE" --no-install-ghc --resolver ghc-8.6.5 runghc foo.hs
