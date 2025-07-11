<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Travis CI

This page documents how to use Stack on
[Travis CI](https://travis-ci.org/). We assume you have basic
familiarity with Travis. We provide two fully baked example files
ready to be used on your projects:

* [The simple Travis configuration](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/resources/travis-simple.yml)
  is intended for applications that do not require multiple GHC
  support or cross-platform support. It builds and tests your project
  with just the settings present in your `stack.yaml` file.
* [The complex Travis configuration](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/resources/travis-complex.yml)
  is intended for projects that need to support multiple GHC versions
  and multiple OSes, such as open source libraries to be released to
  Hackage. It tests against cabal-install, as well as Stack on Linux
  and macOS. The configuration is significantly more involved to allow
  for all of this branching behavior.

    !!! note
        It is likely going to be necessary to modify this configuration to match
        the needs of your project, such as tweaking the build matrix to alter
        which GHC versions you test against, or to specify GHC-version-specific
        `stack.yaml` files if necessary. Do not be surprised if it does not work
        the first time around. See the multiple GHC section below for more
        information.

Each of these configurations is ready to be used immediately, just
copy-paste the content into the `.travis.yml` file in the root or your
repo, enable Travis on the repo, and you are good to go.

You may also be interested in using AppVeyor, which supports Windows
builds, for more cross-platform testing. There is a
[short blog post available on how to do this](http://www.snoyman.com/blog/2016/08/appveyor-haskell-windows-ci),
or just copy in
[the appveyor.yml file](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/resources/appveyor.yml)

The rest of this document explains the details of common Travis
configurations for those of you who want to tweak the above
configuration files or write your own.

*Note:* both Travis and Stack infrastructures are actively developed. We try to
 document best practices at the moment.

## Container infrastructure

For Stack on Travis to be practical, we must use caching. Otherwise build times
will take an incredibly long time, about 30 minutes versus 3-5. Caching is
currently available only for
[container-based Travis infrastructure](http://docs.travis-ci.com/user/workers/container-based-infrastructure/).
Shortly we have to add

~~~yaml
sudo: false

# Caching so the next build will be fast too.
cache:
  directories:
  - $HOME/.stack
~~~

To the `.travis.yml`. This however restricts how we can install GHC and Stack on
the Travis machines.

## Installing Stack

Currently there is only one reasonable way to install Stack: fetch precompiled
binary from the GitHub.

~~~yaml
before_install:
# Download and unpack the stack executable
- mkdir -p ~/.local/bin
- export PATH=$HOME/.local/bin:$PATH
- travis_retry curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
~~~

## Installing GHC

There are two ways to install GHC:

- Let Stack download GHC
- Install GHC using [apt plugin](http://docs.travis-ci.com/user/apt/)

See the above scripts for an example of the first option (letting Stack
download GHC). Here, we will explain the second option. With single GHC the
situation is simple:

~~~yaml
before_install:
  # Install stack as above
  # ...
  # Configure stack to use the system GHC installation
  - stack config set system-ghc --global true
  - export PATH=/opt/ghc/7.10.2/bin:$PATH

addons:
  apt:
    sources:
    - hvr-ghc
    packages:
    - ghc-7.10.2
~~~

### Multiple GHC - parametrised builds

Travis apt plugin does not yet support installing apt packages dynamically
(https://github.com/travis-ci/travis-ci/issues/4291). That for we need to write
a bit repetitive `.travis.yml`.

Also for different GHC versions, you probably want to use different `stack.yaml`
files.

~~~yaml
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
~~~

Especially to use ghc `HEAD` you need to pass `--skip-ghc-check` option to Stack.

## Running tests

After the environment setup, actual test running is simple:

~~~yaml
script:
  - stack --no-terminal --skip-ghc-check test
~~~

In case you are wondering: we need `--no-terminal` because stack does some fancy
sticky display on smart terminals to give nicer status and progress messages,
and the terminal detection is broken on Travis.

## Other details

Some Stack commands will run for long time (when cache is cold) without
producing any output. To avoid timeouts, use the built in [travis_wait](https://docs.travis-ci.com/user/common-build-problems/#Build-times-out-because-no-output-was-received).


~~~yaml
install:
  - travis_wait stack --no-terminal --skip-ghc-check setup
  - travis_wait stack --no-terminal --skip-ghc-check test --only-snapshot
~~~

## Examples

- [futurice/fum2github](https://github.com/futurice/fum2github/blob/master/.travis.yml)
- [haskell-distributed/cloud-haskell](https://github.com/haskell-distributed/cloud-haskell/blob/master/.travis.yml)
- [simonmichael/hledger](https://github.com/simonmichael/hledger/blob/master/.travis.yml)
- [fpco/wai-middleware-crowd](https://github.com/fpco/wai-middleware-crowd/blob/master/.travis.yml)
- [commercialhaskell/all-cabal-hashes-tool](https://github.com/commercialhaskell/all-cabal-hashes-tool/blob/master/.travis.yml)
