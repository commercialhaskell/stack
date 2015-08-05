# Travis

This page documents how to use stack on [travis](https://travis-ci.org/). We assume you have basic familiarity with travis. 

*Note:* both travis and stack infrastructures are actively developed. We try to document best practices at the moment.

## Container infrastructure

For stack on travis to be practical, we must use caching. Otherwise build times will take incredibly long time, about 30 minutes versus 3&mdash;5. Caching is currently available only for [container-based travis infrastructure](http://docs.travis-ci.com/user/workers/container-based-infrastructure/). Shortly we have to add

```yaml
sudo: false
```

To the `.travis.yml`. This however restricts how we can install GHC &amp; stack on the travis machines.

## Installing stack

Currently there is only one reasonable way to install stack: fetch precompiled binary from the github.

```yaml
before_install:
  # stack
  - mkdir -p ~/.local/bin
  - export PATH=~/.local/bin:$PATH
  - travis_retry curl -L https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-linux.gz | gunzip > ~/.local/bin/stack
  - chmod a+x ~/.local/bin/stack
```

*TODO*: https://github.com/travis-ci/travis-ci/issues/4169 is not yet resolved. Otherwise it will be possible to use [apt plugin](http://docs.travis-ci.com/user/apt/).

## Installing GHC

There are two ways to install GHC:
- Let stack download GHC
- Install GHC using [apt plugin](http://docs.travis-ci.com/user/apt/)

We will explain the second option. With single GHC the situation is simple:

```yaml
before_install:
  - export PATH=/opt/ghc/7.8.4/bin:$PATH

addons:
  apt:
    sources:
    - hvr-ghc
    packages:
    - ghc-7.8.4
```

*TODO* is the first option viable? Then we have to install dev dependencies.

If you use one of the recent nightly snapshots which use GHC 7.10.2, you will
need to make sure libgmp is available by adding

```yaml
addons:
  apt:
    packages:
    - libgmp-dev
```

Otherwise the GHC post-install check fails.


### Multiple GHC - parametrised builds

Travis apt plugin doesn't yet support installing apt packages dynamically (https://github.com/travis-ci/travis-ci/issues/4291). That for we need to write a bit repetetive `.travis.yml`.

Also for different GHC versions, you probably want to use different `stack.yaml` files.

```yaml
# N.B. No top-level env: declaration!

matrix:
  include:
  - env: GHCVER=7.8.4 STACK_YAML=stack.yaml
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - ghc-7.8.4
  - env: GHCVER=7.10.1 STACK_YAML=stack-7.10.yaml
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - ghc-7.10.1
  - env: GHCVER=head STACK_YAML=stack-head.yaml
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - ghc-head
  allow_failures:
    - env: GHCVER=head STACK_YAML=stack-head.yaml

before_install:
  # ghc
  - export PATH=/opt/ghc/$GHCVER/bin:$PATH
```

Especially to use ghc `HEAD` you need to pass `--skip-ghc-check` option to stack.

## Running tests

After the environment setup, actual test running is simple:

```yaml
install:
  - ./travis_long stack --no-terminal --skip-ghc-check setup
  - ./travis_long stack --no-terminal --skip-ghc-check test --only-snapshot

script:
  - stack --no-terminal --skip-ghc-check test
```

## Other details

Some stack commands will run for long time (when cache is cold) without producing any output. For travis not to timeout, one can wrap commands in [a simple script](https://github.com/futurice/fum2github/blob/master/travis_long).

```yaml
install:
  - ./travis_long stack --no-terminal --skip-ghc-check setup
  - ./travis_long stack --no-terminal --skip-ghc-check test --only-snapshot
```

## Examples

- https://github.com/futurice/fum2github/blob/master/.travis.yml
- https://github.com/haskell-distributed/cloud-haskell/blob/master/.travis.yml
- https://github.com/simonmichael/hledger/blob/master/.travis.yml