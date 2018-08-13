#!/usr/bin/env bash
#TODO: move this logic into `stack --pvp-bounds`
set -eu -o pipefail

stackbin=stack
loweryaml=stack.yaml
if [[ -f stack-nightly.yaml ]]; then
  upperyaml=stack-nightly.yaml
else
  upperyaml=stack.yaml
fi
pkgyaml=package.yaml
pkgyamlorig=$pkgyaml.SDIST-ORIG

rm -f stack.cabal

ver="$(grep '^version: ' "$pkgyaml"|sed "s/^version: '\(.*\)'$/\1/")"
defaultdist="$("$stackbin" path --dist-dir)"
lowerdist="$("$stackbin" --stack-yaml="$loweryaml" path --dist-dir)"
upperdist="$("$stackbin" --stack-yaml="$upperyaml" path --dist-dir)"

rm -f "$lowerdist/stack-$ver.tar.gz"
"$stackbin" --stack-yaml="$loweryaml" sdist . --pvp-bounds=both
rm -rf "$lowerdist/_lower" && mkdir "$lowerdist/_lower" && (cd "$lowerdist/_lower" && tar xf "../stack-$ver.tar.gz")
lcabal="$(echo "$lowerdist/_lower/stack-$ver/stack.cabal")"
rm -f "$upperdist/stack-$ver.tar.gz"
"$stackbin" --stack-yaml="$upperyaml" sdist . --pvp-bounds=both
rm -rf "$upperdist/_upper" && mkdir "$upperdist/_upper" && (cd "$upperdist/_upper" && tar xf "../stack-$ver.tar.gz")
ucabal="$(echo "$upperdist/_upper/stack-$ver/stack.cabal")"

mv "$pkgyaml" "$pkgyamlorig"
trap 'mv "$pkgyamlorig" "$pkgyaml"; rm -f stack.cabal; rm -f "$pkgyaml.sdist-bak"' EXIT
cp "$pkgyamlorig" "$pkgyaml"

while read -r line; do
  if [[ ( "$line" == *'>'* || "$line" == *'=='* ) && "$line" != *':'* ]]; then
    pkg="$(printf '%s' "$line"|sed 's/^\([a-zA-Z0-9-]*\).*/\1/')"
    l="$(printf '%s' "$line"|sed -e 's/^[a-zA-Z0-9-]* \([0-9*>=<. &]*\),\{0,1\}$/\1/' -e 's/&/\\\&/g')"
    u="$(grep '^ *'"$pkg"' [0-9*>=<. &]*,\{0,1\}$' "$ucabal"|sed -e 's/^ *[a-zA-Z0-9-]* \([0-9*>=<. &]*\),\{0,1\}$/\1/' -e 's/&/\\\&/g'|uniq)"
    echo "pkg=$pkg lower=$l upper=$u"
    if [[ "$l" == "$u" ]]; then
      sed -i.sdist-bak 's/^\( *- '"$pkg"'\) \{0,1\}[0-9*>=<. &]*$/\1 '"$l"'/' "$pkgyaml"
    else
      sed -i.sdist-bak 's/^\( *- '"$pkg"'\) \{0,1\}[0-9*>=<. &]*$/\1 '"($l)"' || '"($u)"'/' "$pkgyaml"
    fi
  fi
done <"$lcabal"

"$stackbin" sdist .
echo
cp "$defaultdist/stack-$ver.tar.gz" "_release/stack-$ver-sdist-1.tar.gz"
echo "SDIST COPIED TO: _release/stack-$ver-sdist-1.tar.gz"
rm -rf "$defaultdist/_both" && mkdir "$defaultdist/_both" && (cd "$defaultdist/_both" && tar xf "../stack-$ver.tar.gz")
cp "$defaultdist/_both/stack-$ver/stack.cabal" "_release/stack-${ver}_bounds.cabal"
echo "CABAL SPEC COPIED TO: _release/stack-${ver}_bounds.cabal"
