# GHCJS

To use GHCJS with stack, place a GHCJS version in the [`compiler`](yaml_configuration.md#compiler) field of `stack.yaml`.  After this, all stack commands should work with GHCJS!  In particular:

* `stack setup` will install GHCJS from source and boot it, which takes a long time.

* `stack build` will compile your code to JavaScript.  In particular, the generated code for an executable ends up in `$(stack path --local-install-root)/bin/EXECUTABLE.jsexe/all.js` (bash syntax, where `EXECUTABLE` is the name of your executable).

You can also build existing stack projects which target GHC, and instead build
them with GHCJS.  For example: `stack build --compiler ghcjs-0.2.0.9006020_ghc-7.10.3`

Sidenote: If you receive a message like `The program 'ghcjs' version >=0.1 is
required but the version of .../ghcjs could not be determined.`, then you may
need to install a different version of `node`. See
[issue #1496](https://github.com/commercialhaskell/stack/issues/1496).

## Example Configurations

### Recent versions of GHCJS, repacked for stack

These versions of GHCJS were created by
[Marcin Tolysz](https://github.com/tolysz), and were particularly crafted to
include package versions which match those expected by particular stackage
snapshots.

For `ghcjs` based on `ghc-7.10.3` one could try:
```yaml
resolver: lts-6.25
compiler: ghcjs-0.2.0.9006025_ghc-7.10.3
compiler-check: match-exact

setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.9006025_ghc-7.10.3:
         url: http://ghcjs.tolysz.org/lts-6.25-9006025.tar.gz
         sha1: 3c87228579b55c05e227a7876682c2a7d4c9c007
```

Or for the latest one based on `ghc-8.0.1` (with more features):
```yaml
resolver: lts-7.14
compiler: ghcjs-0.2.1.9007014_ghc-8.0.1
compiler-check: match-exact

setup-info:
  ghcjs:
    source:
      ghcjs-0.2.1.9007014_ghc-8.0.1:
           url: http://ghcjs.tolysz.org/ghc-8.0-2016-12-25-lts-7.14-9007014.tar.gz
           sha1: 0d2ebe0931b29adca7cb9d9b9f77d60095bfb864
```
The later can be generated via: https://github.com/tolysz/prepare-ghcjs
the fromer is a bit more manual. Those bundles are only tested against the latest `node-7.2.1`.

In order to corrrectly boot and use ghcjs, one might need to install `alex` `happy` `hscolour` `hsc2hs` with the normal ghc.

Older resolvers:

|resolver|url|sha1|
|---|---|---|
| lts-7.13 | http://ghcjs.tolysz.org/ghc-8.0-2016-12-18-lts-7.13-9007013.tar.gz | 530c4ee5e19e2874e128431c7ad421e336df0303 |
| lts-7.8 | http://ghcjs.tolysz.org/ghc-8.0-2016-11-07-lts-7.8-9007008.tar.gz | 190300a3725cde44b2a08be9ef829f2077bf8825 |
| lts-7.7 | http://ghcjs.tolysz.org/ghc-8.0-2016-11-03-lts-7.7-9007007.tar.gz | ce169f85f1c49ad613ae77fc494d5565452ff59a |
| lts-7.5 | http://ghcjs.tolysz.org/ghc-8.0-2016-10-24-lts-7.5-9007005.tar.gz | 450e81028d7f1eb82a16bc4b0809f30730c3e173 |
| lts-7.4 | http://ghcjs.tolysz.org/ghc-8.0-2016-10-22-lts-7.4-9007004.tar.gz | ed77b3c15fedbadad5ab0e0afe1bd42c0a8695b4 |
| lts-7.3 | http://ghcjs.tolysz.org/ghc-8.0-2016-10-11-lts-7.3-9007003.tar.gz | 3196fd5eaed670416083cf3678396d02c50096de |
| lts-7.2 | http://ghcjs.tolysz.org/ghc-8.0-2016-10-01-lts-7.2-9007002.tar.gz | a41ae415328e2b257d40724d13d1386390c26322 | 
| lts-7.1 | http://ghcjs.tolysz.org/ghc-8.0-2016-09-26-lts-7.1-9007001-mem.tar.gz | e640724883238593e2d2f7f03991cb413ec0347b |
| lts-6.21 | http://ghcjs.tolysz.org/lts-6.21-9006021.tar.gz | 80b83f85dcec182093418e843979f4cee092fa85 |
| lts-6.20 | http://ghcjs.tolysz.org/lts-6.20-9006020.tar.gz | a6cea90cd8121eee3afb201183c6e9bd6bacd94a |
| lts-6.19 | http://ghcjs.tolysz.org/lts-6.19-9006019.tar.gz | ef4264d5a93b269ee4ec8f9d5139da030331d65a |
| lts-6.18 | http://ghcjs.tolysz.org/lts-6.18-9006018.tar.gz | 3e9f345116c851349a5a551ffd94f7e0b74bfabb |

If you do not use the same resolver, say, an older LTS snapshot, you will get
some warnings like this:

```
Ignoring that the GHCJS boot package "aeson" has a different version, 0.9.0.1, than the resolver's wanted version, 0.8.0.2
Ignoring that the GHCJS boot package "attoparsec" has a different version, 0.13.0.1, than the resolver's wanted version, 0.12.1.6
Ignoring that the GHCJS boot package "scientific" has a different version, 0.3.3.8, than the resolver's wanted version, 0.3.4.2
...
```

These warnings can usually be safely ignored, but they do indicate a divergence
between your snapshot's packages, and those that are being used to compile your
project. You will normally get these warnings when using a GHCJS tarball that
has not been packaged with a particular snapshot in mind.

### GHCJS (old base)

If you want to build some older GHCJS packages, you may need to use the "old
base" GHCJS.  To do this, use the following compiler info:

```yaml
compiler: ghcjs-0.1.0.20150924_ghc-7.10.2
compiler-check: match-exact
```

### Custom installed GHCJS (development branch)

In order to use a GHCJS installed on your path, just add the following to your `stack.yaml`:

```yaml
compiler: ghcjs-0.2.0_ghc-7.10.2
```

(Or, `ghcjs-0.1.0_ghc-7.10.2` if you are working with an older version)

## Project with both client and server

For projects with both a server and client, the recommended project
organization is to put one or both of your `stack.yaml` files in
sub-directories.  This way, you can use the current working directory to
specify whether you're working on the client or server.  This will also allow
more straightforward editor tooling, once projects like `ghc-mod` and
`haskell-ide-engine` support GHCJS.

For example, here's what a script for building both client and server looks
like:

```bash
#!/bin/bash

# Build the client
stack build --stack-yaml=client/stack.yaml

# Copy over the javascript
rm -f server/static/all.js
cp $(stack path --stack-yaml=client/stack.yaml --local-install-root)/bin/client.jsexe/all.js server/static/all.js

# Build the server
stack build --stack-yaml=server/stack.yaml
```

You can also put both the yaml files in the same directory, and have e.g.
`ghcjs-stack.yaml`, but this won't work well with editor integrations.

## Using stack without a snapshot

If you don't want to use a snapshot, instead place the ghcjs version in the `resolver` field of your `stack.yaml`.  This is also necessary when using stack `< 0.1.8`.

## Setting up GHCJS on Windows

If `stack setup` command fails to successfully complete with message: `commitBuffer: invalid argument (invalid character)`, it means you have a locale problem.
This problem is not exclusive to GHCJS, and might happen also during other builds. A workaround is to set _Language for non-Unicode programs_ to _English (US)_.
For details see [stack issue #1448](https://github.com/commercialhaskell/stack/issues/1448).
