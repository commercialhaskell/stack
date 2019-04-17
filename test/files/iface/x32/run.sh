#!/usr/bin/env bash

set -eux

go() {
  for ver in 7.10.3 8.0.2 8.2.2 8.4.4 8.6.4
  do
      stack --resolver ghc-$ver --arch i386 ghc -- -fforce-recomp Main.hs
      local DIR
      DIR=ghc"$(echo $ver | tr -d '.')"
      mkdir -p DIR
      mv Main.hi $DIR/Main.hi
  done
}

go
