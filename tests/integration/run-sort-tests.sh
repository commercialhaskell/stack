#!/usr/bin/env bash

set -uo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

export STACK_ROOT=$HOME/.stack

DIR=$(pwd)
STACK=$(stack exec which stack)

export SRC_DIR=$DIR/../../

mkdir -p tests-success
mkdir -p tests-fail
mkdir -p logs

cd "$DIR/tests"
for f in *
do
    cd "$DIR/tests"
    if [[ -d "$f" ]]
    then
        mkdir -p "$f/files"
        cd "$f/files"
        echo Running test $f
        export TEST_DIR=$DIR/tests/$f
        $STACK --stack-yaml $DIR/../../stack.yaml runghc --no-ghc-package-path -- -i../../../lib ../Main.hs > $DIR/logs/$f 2>&1
        RES=$?
        cd "$DIR/tests"
        echo Result code for $f: $RES
        if [[ $RES -eq 0 ]]
        then
            mv "$f" ../tests-success
            rm $DIR/logs/$f
        else
            mv "$f" ../tests-fail
        fi
    fi
done
