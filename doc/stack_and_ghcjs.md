# Stack & GHCJS

To set up and use GHCJS with stack, you should place GHCJS `resolver` in your project's `stack.yaml` (see instructions for [old base](#ghcjs-old-base) and [`master` - a.k.a. improved base](#ghcjs-master-aka-improved-base)).  Once this has been done, Stack will use GHCJS for most commands, with the exception of  `ghc`, `runghc` and `ide`.  (Support for `ghc` and `runghc` will likely be added before the next release - see [#1054](https://github.com/commercialhaskell/stack/issues/1054)).

After creating a `stack.yaml`, which specifies that ghcjs is to be used (see below), you can have Stack automatically set up and boot GHCJS. Stack must be newer than `0.1.6`:

```
$ stack setup
```

(this will take a long time)

## Using a stackage snapshot

The configurations below use a compiler resolver, which means that you can't use packages from a stackage snapshot.  The latest development version (which will probably be released as `0.1.7`) also supports using GHCJS with a stackage snapshot, via a `compiler` field.  To use the latest development version, do the following:

```
$ stack upgrade --git
```

For example, the old-base configuration looks like this:

```yaml
resolver: lts-3.10
compiler: ghcjs-0.1.0.20150924_ghc-7.10.2
compiler-check: match-exact
```

You can also build existing stack projects which target GHC and instead build them with GHCJS.  To do this, invoke stack like this: `stack build --compiler ghcjs-0.1.0.20150924_ghc-7.10.2`

## Example Configurations

### GHCJS (old base)

You can use this resolver for GHCJS (old base) in your `stack.yaml`:

```yaml
resolver: ghcjs-0.1.0.20150924_ghc-7.10.2
compiler-check: match-exact
```

### GHCJS `master` (a.k.a. improved base)

To use the master branch, a.k.a improved base, add the following to your `stack.yaml`:

```yaml
resolver: ghcjs-0.2.0.20151001_ghc-7.10.2
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.20151001_ghc-7.10.2:
        url: "https://github.com/fizruk/ghcjs/releases/download/v0.2.0.20151001/ghcjs-0.2.0.20151001.tar.gz"
```

or for the 2015-10-29 master branch 
```yaml
resolver: ghcjs-0.2.0.20151029_ghc-7.10.2
compiler-check: match-exact
setup-info:
  ghcjs:
      source:
            ghcjs-0.2.0.20151029_ghc-7.10.2:
                    url: "https://github.com/nrolland/ghcjs/releases/download/v0.2.0.20151029/ghcjs-0.2.0.20151029.tar.gz"
```

### Custom installed GHCJS (development branch)

In order to use a GHCJS installed on your path, just add the following to your `stack.yaml`:

```yaml
resolver: ghcjs-0.2.0_ghc-7.10.2
```

(Or, `ghcjs-0.1.0_ghc-7.10.2` if you are working with an older version)
