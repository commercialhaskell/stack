# GHCJS

To use GHCJS with stack `>= 0.1.8`, place a GHCJS version in the [`compiler`](yaml_configuration.md#compiler) field of `stack.yaml`.  After this, all stack commands should work with GHCJS, except for `ide`.  In particular:

* `stack setup` will install GHCJS from source and boot it, which takes a long time.
* `stack build` will compile your code to JavaScript.  In particular, the generated code for an executable ends up in `$(stack path --local-install-root)/bin/EXECUTABLE.jsexe/all.js` (bash syntax, where `EXECUTABLE` is the name of your executable).

You can also build existing stack projects which target GHC, and instead build them with GHCJS.  For example: `stack build --compiler ghcjs-0.1.0.20150924_ghc-7.10.2`

Sidenote: If you receive a message like `The program 'ghcjs' version >=0.1 is
required but the version of .../ghcjs could not be determined.`, then you may
need to install a different version of `node`. See
[stack issue #1496](https://github.com/commercialhaskell/stack/issues/1496).

## Example Configurations

### GHCJS (old base)

You can use this resolver for GHCJS (old base) in your `stack.yaml`:

```yaml
compiler: ghcjs-0.1.0.20150924_ghc-7.10.2
compiler-check: match-exact
```

### GHCJS `master` (a.k.a. improved base)

To use the master branch, a.k.a improved base, add the following to your `stack.yaml`:

```yaml
compiler: ghcjs-0.2.0.20151230.3_ghc-7.10.2
compiler-check: match-exact
setup-info:
 ghcjs:
  source:
   ghcjs-0.2.0.20151230.3_ghc-7.10.2:
    url: "https://github.com/nrolland/ghcjs/releases/download/v.0.2.0.20151230.3/ghcjs-0.2.0.20151230.3.tar.gz"
```

or for the 2015-10-29 version
```yaml
compiler: ghcjs-0.2.0.20151029_ghc-7.10.2
compiler-check: match-exact
setup-info:
  ghcjs:
      source:
            ghcjs-0.2.0.20151029_ghc-7.10.2:
                    url: "https://github.com/nrolland/ghcjs/releases/download/v0.2.0.20151029/ghcjs-0.2.0.20151029.tar.gz"
```

or for the 2015-10-01 version
```yaml
compiler: ghcjs-0.2.0.20151001_ghc-7.10.2
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.20151001_ghc-7.10.2:
        url: "https://github.com/fizruk/ghcjs/releases/download/v0.2.0.20151001/ghcjs-0.2.0.20151001.tar.gz"
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
