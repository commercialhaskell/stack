#!/bin/sh

set -eux

mkdir -p ~/.local/bin;
if [ `uname` = "Darwin" ]; then
  curl -kL https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin;
else
  curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
fi;
stack --no-terminal setup;

case "$BUILD" in
  stack)
    ;;
  cabal)
mkdir -p $HOME/.cabal
cat > $HOME/.cabal/config <<EOF
remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/
remote-repo-cache: $HOME/.cabal/packages
jobs: \$ncpus
EOF
;;
esac
