#!/usr/bin/env bash
set -xe
STACKVER=$(cat stack.cabal|grep '^version:'|head -1|awk '{print $2}')
STACKDOCDIR=stack-$STACKVER-docs
stack haddock
rm -rf _release/$STACKDOCDIR
mkdir -p _release
cp -r $(stack path --local-doc-root)/stack-$STACKVER _release/$STACKDOCDIR
sed -i '' 's/href="\.\.\/\([^/]*\)\//href="..\/..\/\1\/docs\//g' _release/$STACKDOCDIR/*.html
(cd _release && tar cvz --format=ustar -f $STACKDOCDIR.tar.gz $STACKDOCDIR)
curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u borsboom \
     --data-binary "@_release/$STACKDOCDIR.tar.gz" \
     "https://hackage.haskell.org/package/stack-$STACKVER/docs"
