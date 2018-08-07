#!/usr/bin/env bash

set -eux

cd $(dirname ${BASH_SOURCE[0]})

for d in lts-haskell stackage-nightly stackage-snapshots
do
    if [[ ! -d "$d" ]]
    then
        git clone https://github.com/commercialhaskell/$d
    fi
done

stack build :convert-old-stackage --exec convert-old-stackage
