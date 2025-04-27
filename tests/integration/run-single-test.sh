#!/usr/bin/env bash

set -uo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

export STACK_ROOT=$HOME/.stack
unset GHC_PACKAGE_PATH

DIR=$(pwd)
STACK=$(stack exec which stack)

export SRC_DIR=$DIR/../../
export TEST_DIR=$DIR/tests/$1

if [[ ! -d "tests/$1" ]]
then
    echo Test does not exist: $1
    exit 1
fi

mkdir -p tests/$1/files
cd tests/$1/files
echo Running test $1
exec $STACK --stack-yaml $DIR/../../stack.yaml runghc --no-ghc-package-path -- -i../../../lib ../Main.hs
