#!/usr/bin/env bash
#TODO: move this logic into `stack --pvp-bounds`
set -eu -o pipefail

rootdir="$(cd "$(dirname "$0")/../.."; pwd)"
stackbin=stack
loweryaml=$rootdir/stack.yaml
if [[ -f $rootdir/stack-lts-12.yaml ]]; then
  midyaml=$rootdir/stack-lts-12.yaml
else
  midyaml=$rootdir/stack.yaml
fi
if [[ -f $rootdir/stack-nightly.yaml ]]; then
  upperyaml=$rootdir/stack-nightly.yaml
else
  upperyaml=$rootdir/stack.yaml
fi
pkgyaml=package.yaml
pkgyamlorig=$pkgyaml.SDIST-ORIG

pkgver="$(grep '^version:' "$pkgyaml"|sed "s/^version: *'*\([^']*\)'*$/\1/")"
pkgname="$(grep '^name:' "$pkgyaml"|sed "s/^name: *'*\([^']*\)'*$/\1/")"
defaultdist="$("$stackbin" path --dist-dir)"
lowerdist="$("$stackbin" --stack-yaml="$loweryaml" path --dist-dir)"
middist="$("$stackbin" --stack-yaml="$midyaml" path --dist-dir)"
upperdist="$("$stackbin" --stack-yaml="$upperyaml" path --dist-dir)"

rm -f $pkgname.cabal
rm -f "$lowerdist/$pkgname-$pkgver.tar.gz"
"$stackbin" --stack-yaml="$loweryaml" sdist . --pvp-bounds=both
rm -rf "$lowerdist/_lower" && mkdir "$lowerdist/_lower" && (cd "$lowerdist/_lower" && tar xf "../$pkgname-$pkgver.tar.gz")
lcabal="$(echo "$lowerdist/_lower/$pkgname-$pkgver/$pkgname.cabal")"
rm -f "$middist/$pkgname-$pkgver.tar.gz"
"$stackbin" --stack-yaml="$midyaml" sdist . --pvp-bounds=both
rm -rf "$middist/_mid" && mkdir "$middist/_mid" && (cd "$middist/_mid" && tar xf "../$pkgname-$pkgver.tar.gz")
mcabal="$(echo "$middist/_mid/$pkgname-$pkgver/$pkgname.cabal")"
rm -f "$upperdist/$pkgname-$pkgver.tar.gz"
"$stackbin" --stack-yaml="$upperyaml" sdist . --pvp-bounds=both
rm -rf "$upperdist/_upper" && mkdir "$upperdist/_upper" && (cd "$upperdist/_upper" && tar xf "../$pkgname-$pkgver.tar.gz")
ucabal="$(echo "$upperdist/_upper/$pkgname-$pkgver/$pkgname.cabal")"

mv "$pkgyaml" "$pkgyamlorig"
trap 'mv "$pkgyamlorig" "$pkgyaml"; rm -f $pkgname.cabal; rm -f "$pkgyaml.sdist-bak"' EXIT
cp "$pkgyamlorig" "$pkgyaml"

while read -r line; do
  if [[ ( "$line" == *'>'* || "$line" == *'=='* ) && "$line" != *':'* ]]; then
    pkg="$(printf '%s' "$line"|sed 's/^\([a-zA-Z0-9-]*\).*/\1/')"
    l="$(printf '%s' "$line"|sed -e 's/^[a-zA-Z0-9-]* \([0-9*>=<. &]*\),\{0,1\}$/\1/' -e 's/&/\\\&/g')"
    m="$(grep '^ *'"$pkg"' [0-9*>=<. &]*,\{0,1\}$' "$mcabal"|sed -e 's/^ *[a-zA-Z0-9-]* \([0-9*>=<. &]*\),\{0,1\}$/\1/' -e 's/&/\\\&/g'|uniq)"
    u="$(grep '^ *'"$pkg"' [0-9*>=<. &]*,\{0,1\}$' "$ucabal"|sed -e 's/^ *[a-zA-Z0-9-]* \([0-9*>=<. &]*\),\{0,1\}$/\1/' -e 's/&/\\\&/g'|uniq)"
    echo "pkg=$pkg lower=$l mid=$m upper=$u"
    if [[ "$l" == "$u" && "$l" == "$m" ]]; then
      sed -i.sdist-bak 's/^\( *- '"$pkg"'\) \{0,1\}[0-9*>=<. &]*$/\1 '"$l"'/' "$pkgyaml"
    elif [[ "$l" == "$m" || "$m" == "$u" ]]; then
      sed -i.sdist-bak 's/^\( *- '"$pkg"'\) \{0,1\}[0-9*>=<. &]*$/\1 '"($l)"' || '"($u)"'/' "$pkgyaml"
    else
      sed -i.sdist-bak 's/^\( *- '"$pkg"'\) \{0,1\}[0-9*>=<. &]*$/\1 '"($l)"' || '"($m)"' || '"($u)"'/' "$pkgyaml"
    fi
  fi
done <"$lcabal"

"$stackbin" sdist .
echo
cp "$defaultdist/$pkgname-$pkgver.tar.gz" "$rootdir/_release/$pkgname-$pkgver-sdist-1.tar.gz"
echo "SDIST COPIED TO: $rootdir/_release/$pkgname-$pkgver-sdist-1.tar.gz"
rm -rf "$defaultdist/_both" && mkdir "$defaultdist/_both" && (cd "$defaultdist/_both" && tar xf "../$pkgname-$pkgver.tar.gz")
cp "$defaultdist/_both/$pkgname-$pkgver/$pkgname.cabal" "$rootdir/_release/$pkgname-${pkgver}_bounds.cabal"
echo "CABAL SPEC COPIED TO: $rootdir/_release/$pkgname-${pkgver}_bounds.cabal"
