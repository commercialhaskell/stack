# GHCJS

To use GHCJS with stack `>= 0.1.8`, place a GHCJS version in the
[`compiler`](yaml_configuration.md#compiler) field of `stack.yaml`.  After
this, all stack commands should work with GHCJS, except for `ide`.  In
particular:

* `stack setup` will install GHCJS from source and boot it, which takes a long
  time.
* `stack build` will compile your code to JavaScript.  In particular, the
  generated code for an executable ends up in `$(stack path
  --local-install-root)/bin/EXECUTABLE.jsexe/all.js` (bash syntax, where
  `EXECUTABLE` is the name of your executable).

You can also build existing stack projects which target GHC, and instead build
them with GHCJS.  For example: `stack build --compiler
ghcjs-0.1.0.20150924_ghc-7.10.2`

Sidenote: If you receive a message like `The program 'ghcjs' version >=0.1 is
required but the version of .../ghcjs could not be determined.`, then you may
need to install a different version of `node`. See [stack issue
#1496](https://github.com/commercialhaskell/stack/issues/1496).

## Example Configurations

### GHCJS `master` (a.k.a. improved base)

To use the master branch, a.k.a improved base, add the following to your `stack.yaml`:

GHCJS compiled with GHC 7.10.3 LTS-5.12 (stack.yaml upgraded from stock GHCJS)
```yaml
compiler: ghcjs-0.2.0.20160414_ghc-7.10.3
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.20160414_ghc-7.10.3:
        url: https://s3.amazonaws.com/ghcjs/ghcjs-0.2.0.20160414_ghc-7.10.3.tar.gz
        sha1: 6d6f307503be9e94e0c96ef1308c7cf224d06be3
```

GHCJS compiled with GHC 7.10.2 LTS-3.6 (stack.yaml that comes with GHCJS)
```yaml
compiler: ghcjs-0.2.0.20160414_ghc-7.10.2
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.20160414_ghc-7.10.2:
        url: https://s3.amazonaws.com/ghcjs/ghcjs-0.2.0.20160414_ghc-7.10.2.tar.gz
        sha1: f0a7243e781e27ebfe601eebaf5c57422007c142
```

### GHCJS (old base)

You can use this resolver for GHCJS (old base) in your `stack.yaml`:

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

## Comunity Repacks

Each version of resolvers can potentially have diferent versions of packages that the upstream bundle has.
One way of remeding this situation is to repack the bundle and include packages at version from the resolver.
Until `ghcjs` will recognise `stack` we need to install a separate version of `ghcjs` for each resolver. In the future we would need to reinstall only if the version of booted packages change or maybe just boot it for the new resolver.

However it is only a possible future development.
Currently, for more recent snapshots one could try:
```yaml
resolver: lts-6.19
compiler: ghcjs-0.2.0.9006019_ghc-7.10.3
compiler-check: match-exact

setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.9006019_ghc-7.10.3:
         url: http://tolysz.org/ghcjs/lts-6.19-9006019.tar.gz
         sha1: ef4264d5a93b269ee4ec8f9d5139da030331d65a
```

Or for the latest (with more features):
```yaml
resolver: lts-7.1
compiler: ghcjs-0.2.1.9007001_ghc-8.0.1
compiler-check: match-exact

setup-info:
  ghcjs:
    source:
      ghcjs-0.2.1.9007001_ghc-8.0.1:
          url: http://tolysz.org/ghcjs/ghc-8.0-2016-09-26-lts-7.1-9007001-mem.tar.gz
          sha1: e640724883238593e2d2f7f03991cb413ec0347b
```
The later can be generated via: https://github.com/tolysz/prepare-ghcjs
the fromer is a bit more manual.

Older resolvers:

|resolver|url|sha1|
|---|---|---|
| lts-6.18 | http://tolysz.org/ghcjs/lts-6.18-9006019.tar.gz | 3e9f345116c851349a5a551ffd94f7e0b74bfabb |

## Setting up GHCJS on Windows

If `stack setup` command fails to successfully complete with message: `commitBuffer: invalid argument (invalid character)`, it means you have a locale problem.
This problem is not exclusive to GHCJS, and might happen also during other builds. A workaround is to set _Language for non-Unicode programs_ to _English (US)_.
For details see [stack issue #1448](https://github.com/commercialhaskell/stack/issues/1448).
