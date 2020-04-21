#!/bin/sh

set -eux

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_stack_osx() {
  curl -skL https://get.haskellstack.org/stable/osx-x86_64.tar.gz | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin;
}

fetch_stack_linux() {
  curl -sL https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
}

# We need stack to generate cabal files with precise bounds, even for cabal
# builds.
mkdir -p ~/.local/bin;
if [ `uname` = "Darwin" ]; then
  travis_retry fetch_stack_osx
else
  travis_retry fetch_stack_linux
fi
