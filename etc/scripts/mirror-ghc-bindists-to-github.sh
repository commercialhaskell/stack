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
#
GHCVER=8.0.2
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
echo 'ghc:' >stack-setup.yaml

mirror () {
  local srcfn=ghc-$GHCVER-$2.tar.$3
  if [[ ! -s "$srcfn.downloaded" ]]; then
    rm -f "$srcfn"
    curl -LO --fail "http://downloads.haskell.org/~ghc/$GHCVER/$srcfn"
    date >"$srcfn.downloaded"
  fi
  local destfn=ghc-$GHCVER-$2.tar.$4
  if [[ ! -s "$destfn.uploaded" ]]; then
    if [[ "$3" == "xz" && "$4" == "bz2" ]]; then
      xzcat "$srcfn" | bzip2 -c > "$destfn"
    elif [[ "$3" != "$4" ]]; then
      set +x
      echo
      echo "$0: Unsupported conversion: $3 to $4" >&2
      exit 1
    fi
    curl -X POST --data-binary "@$destfn" -H "Content-type: application/octet-stream" -H "Authorization: token $GITHUB_AUTH_TOKEN" "$UPLOAD_URL?name=$destfn"
    date >"$destfn.uploaded"
  fi
  echo "    $1:" >>stack-setup.yaml
  echo "        $GHCVER:" >>stack-setup.yaml
  echo "            url: \"https://github.com/commercialhaskell/ghc/releases/download/ghc-$GHCVER-release/$destfn\"" >>stack-setup.yaml
  echo "            content-length: $(stat --printf="%s" "$destfn" 2>/dev/null || stat -f%z "$destfn")" >>stack-setup.yaml
  echo "            sha1: $(sha1sum $destfn |cut -d' ' -f1)" >>stack-setup.yaml
}

mirror linux32 i386-deb7-linux xz xz
mirror linux32-nopie i386-deb8-linux xz xz
mirror linux64 x86_64-deb7-linux xz xz
mirror linux64-nopie x86_64-deb8-linux xz xz
mirror linux32-gmp4 i386-centos67-linux xz xz
mirror linux64-gmp4 x86_64-centos67-linux xz xz
mirror macosx x86_64-apple-darwin xz bz2
mirror windows32 i386-unknown-mingw32 xz xz
mirror windows64 x86_64-unknown-mingw32 xz xz
mirror freebsd32 i386-portbld-freebsd xz xz
mirror freebsd64 x86_64-portbld-freebsd xz xz
mirror openbsd64 x86_64-unknown-openbsd xz xz
mirror linux-armv7 armv7-deb8-linux xz xz

set +x
echo
echo "DONE!  See 'stack-setup.yaml' for 'stack setup' metadata."
