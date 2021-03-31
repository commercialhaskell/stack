#!/usr/bin/env bash
#
# This script will download official GHC bindists from download.haskell.org and upload
# them to the Github Release that Stack uses.
#
# Prerequisites:
#  - Create a Github release with tag `ghc-X.Y.Z-release`
#  - Set GITHUB_AUTH_TOKEN to a token that has permission to upload assets to a Release
#
# To use:
#  - Update GHCVER at the top of the script for the version you are mirring.
#  - Update the `mirror` commands at the bottom with any required filename adjustments
#    and other changes.
#  - Run the script.
#
# The script will output a `stack-setup.yaml` file containing info for each
# platform that you can paste into
# https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml.
# Be sure to double check the SHA1 sums against those in
# https://downloads.haskell.org/~ghc/X.Y.Z/.
#

GHCVER=9.0.1

if [[ -z "$GITHUB_AUTH_TOKEN" ]]; then
  echo "$0: GITHUB_AUTH_TOKEN environment variable is required" >&2
  exit 1
fi
set -xe
UPLOAD_URL="$(curl --fail -sSLH "Authorization: token $GITHUB_AUTH_TOKEN" https://api.github.com/repos/commercialhaskell/ghc/releases/tags/ghc-$GHCVER-release |grep upload_url |head -1 |sed 's/.*"\(https.*assets\){.*/\1/')"
if [[ -z "$UPLOAD_URL" ]]; then
  set +x
  echo
  echo "$0: Could not get upload URL from Github" >&2
  exit 1
fi
echo 'ghc:' >stack-setup-$GHCVER.yaml

mirror_ () {
  base_url="$1"; shift
  suffix="$1"; shift
  destsuffix="$1"; shift
  srcext="$1"; shift
  destext="$1"; shift
  local srcurl="$base_url/ghc-$GHCVER-${suffix}.tar.${srcext}"
  local srcfn=ghc-$GHCVER-${suffix}${destsuffix:+_}${destsuffix}.tar.${srcext}
  if [[ ! -s "$srcfn.downloaded" ]]; then
    rm -f "$srcfn"
    curl -Lo "$srcfn" --fail "$srcurl"
    date >"$srcfn.downloaded"
  fi
  local destfn=ghc-$GHCVER-${suffix}${destsuffix:+_}${destsuffix}.tar.${destext}
  if [[ ! -s "$destfn.uploaded" ]]; then
    if [[ "${srcext}" == "xz" && "${destext}" == "bz2" ]]; then
      xzcat "$srcfn" | bzip2 -c > "$destfn"
    elif [[ "${srcext}" != "${destext}" ]]; then
      set +x
      echo
      echo "$0: Unsupported conversion: ${srcext} to ${destext}" >&2
      exit 1
    fi
    curl --fail -X POST --data-binary "@$destfn" -H "Content-type: application/octet-stream" -H "Authorization: token $GITHUB_AUTH_TOKEN" "$UPLOAD_URL?name=$destfn" |cat
    date >"$destfn.uploaded"
  fi
  while [[ $# -gt 0 ]]; do
    alias="$1"
    echo "    $alias:" >>stack-setup-$GHCVER.yaml
    echo "        $GHCVER:" >>stack-setup-$GHCVER.yaml
    if [[ "$srcfn" == "$destfn" ]]; then
      echo "            url: \"$srcurl\"" >>stack-setup-$GHCVER.yaml
      echo "            #mirror-url: \"https://github.com/commercialhaskell/ghc/releases/download/ghc-$GHCVER-release/$destfn\"" >>stack-setup-$GHCVER.yaml
    else
      echo "            # Converted to $destext from $srcurl" >>stack-setup-$GHCVER.yaml
      echo "            url: \"https://github.com/commercialhaskell/ghc/releases/download/ghc-$GHCVER-release/$destfn\"" >>stack-setup-$GHCVER.yaml
    fi
    echo "            content-length: $(stat --printf="%s" "$destfn" 2>/dev/null || stat -f%z "$destfn")" >>stack-setup-$GHCVER.yaml
    echo "            sha1: $(shasum -a 1 $destfn |cut -d' ' -f1)" >>stack-setup-$GHCVER.yaml
    echo "            sha256: $(shasum -a 256 $destfn |cut -d' ' -f1)" >>stack-setup-$GHCVER.yaml
    echo "" >>stack-setup-$GHCVER.yaml
    shift
  done
}

mirror () {
  mirror_ http://downloads.haskell.org/~ghc/$GHCVER "$@"
}

# NOTE: keep the 'mirror' commands in the same order as entries in
# https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml

mirror i386-deb9-linux "" xz xz linux32
mirror x86_64-deb9-linux "" xz xz linux64
#mirror x86_64-centos67-linux "" xz xz linux64-gmp4
mirror x86_64-fedora27-linux "" xz xz linux64-tinfo6
mirror x86_64-apple-darwin "" bz2 bz2 macosx
#mirror i386-unknown-mingw32 "" xz xz windows32
mirror x86_64-unknown-mingw32 "" xz xz windows64
mirror armv7-deb9-linux "" xz xz linux-armv7
mirror aarch64-deb9-linux "" xz xz linux-aarch64

mirror_ https://github.com/redneb/ghc-alt-libc/releases/download/ghc-$GHCVER-musl i386-unknown-linux-musl "" xz xz linux32-musl
mirror_ https://github.com/redneb/ghc-alt-libc/releases/download/ghc-$GHCVER-musl x86_64-unknown-linux-musl "" xz xz linux64-musl

mirror_ http://distcache.FreeBSD.org/local-distfiles/arrowd/stack-bindists/11 i386-portbld-freebsd "" xz xz freebsd32
mirror_ http://distcache.FreeBSD.org/local-distfiles/arrowd/stack-bindists i386-portbld-freebsd "ino64" xz xz freebsd32-ino64
mirror_ http://distcache.FreeBSD.org/local-distfiles/arrowd/stack-bindists/11 x86_64-portbld-freebsd "" xz xz freebsd64
mirror_ http://distcache.FreeBSD.org/local-distfiles/arrowd/stack-bindists x86_64-portbld-freebsd "ino64" xz xz freebsd64-ino64

set +x
echo
echo "DONE!  See 'stack-setup-$GHCVER.yaml' for 'stack setup' to add to"
echo "  https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml"
