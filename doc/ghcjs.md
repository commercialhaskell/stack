# GHCJS

To use GHCJS with stack, place a GHCJS version in the [`compiler`](yaml_configuration.md#compiler) field of `stack.yaml`.  After this, all stack commands should work with GHCJS!  In particular:

* `stack setup` will install GHCJS from source and boot it, which takes a long time.

* `stack build` will compile your code to JavaScript.  In particular, the generated code for an executable ends up in `$(stack path --local-install-root)/bin/EXECUTABLE.jsexe/all.js` (bash syntax, where `EXECUTABLE` is the name of your executable).

You can also build existing stack projects which target GHC, and instead build
them with GHCJS.  For example: `stack build --compiler ghcjs-0.2.0.9006020_ghc-7.10.3`

Sidenote: If you receive a message like `The program 'ghcjs' version >=0.1 is
required but the version of .../ghcjs could not be determined.`, then you may
need to install a different version of `node`. See
[stack issue #1496](https://github.com/commercialhaskell/stack/issues/1496).

## Example Configurations

### Recent versions of GHCJS, repacked for stack

These versions of GHCJS were created by
[Marcin Tolysz](https://github.com/tolysz), and were particularly crafted to
include package versions which match those expected by particular stackage
snapshots.

For `ghcjs` based on `ghc-7.10.3` one could try:
```yaml
resolver: lts-6.21
compiler: ghcjs-0.2.0.9006021_ghc-7.10.3
compiler-check: match-exact

setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.9006021_ghc-7.10.3:
         url: http://ghcjs.tolysz.org/lts-6.21-9006021.tar.gz
         sha1: 80b83f85dcec182093418e843979f4cee092fa85
```

Or for the latest one based on `ghc-8.0.1` (with more features):
```yaml
resolver: lts-7.3
compiler: ghcjs-0.2.1.9007003_ghc-8.0.1
compiler-check: match-exact

setup-info:
  ghcjs:
    source:
      ghcjs-0.2.1.9007003_ghc-8.0.1:
          url: http://ghcjs.tolysz.org/ghc-8.0-2016-10-11-lts-7.3-9007003.tar.gz
          sha1: 3196fd5eaed670416083cf3678396d02c50096de
```
The later can be generated via: https://github.com/tolysz/prepare-ghcjs
the fromer is a bit more manual. Those bundles are only tested against the latest `node-6.7.0`.

Older resolvers:

|resolver|url|sha1|
|---|---|---|
| lts-7.2 | http://ghcjs.tolysz.org/ghc-8.0-2016-10-01-lts-7.2-9007002.tar.gz | a41ae415328e2b257d40724d13d1386390c26322 | 
| lts-7.1 | http://ghcjs.tolysz.org/ghc-8.0-2016-09-26-lts-7.1-9007001-mem.tar.gz | e640724883238593e2d2f7f03991cb413ec0347b |
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

For projects with both a server and client, the recommended project organization is to put one or both of your `stack.yaml` files in sub-directories.  This way, you can use the current working directory to specify whether you're working on the client or server.  This will also allow more straightforward editor tooling, once projects like `ghc-mod` and `haskell-ide-engine` support GHCJS.

For example, here's what a script for building both client and server looks like:

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

You can also put both the yaml files in the same directory, and have e.g. `ghcjs-stack.yaml`, but this won't work well with editor integrations.

## Using stack without a snapshot

If you don't want to use a snapshot, instead place the ghcjs version in the `resolver` field of your `stack.yaml`.  This is also necessary when using stack `< 0.1.8`.

## Setting up GHCJS on Windows

If `stack setup` command fails to successfully complete with message: `commitBuffer: invalid argument (invalid character)`, it means you have a locale problem.
This problem is not exclusive to GHCJS, and might happen also during other builds. A workaround is to set _Language for non-Unicode programs_ to _English (US)_.
For details see [stack issue #1448](https://github.com/commercialhaskell/stack/issues/1448).
