#!/usr/bin/env bash

set -eux

cd $(dirname ${BASH_SOURCE[0]})

for d in lts-haskell stackage-nightly stackage-snapshots
do
    if [[ ! -d "$d" ]]
    then
        git clone https://github.com/commercialhaskell/$d
    else
        (cd "$d" && git pull || echo "Git pull failed, ignoring")
    fi
done

stack build --flag pantry:convert-old-stackage pantry:convert-old-stackage --exec convert-old-stackage
