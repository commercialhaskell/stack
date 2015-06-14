This page is intended to fully document all configuration options available in the stack.yaml file. Note that, as we're still in beta, this page is likely to be both *incomplete* and sometimes *inaccurate*. If you see such cases, please update the page, and if you're not sure how, open an issue labeled "question".

The stack.yaml configuration options break down into project specific and non-project specific options. The latter can be specified in your global config (/etc/stack/config) and user config (~/.stack/stack.yaml), as well as in the project config. The former must be in the project config.

## Project config

### packages

### extra-deps

### resolver

### flags
Flags can be set for each package separately, e.g.
```
flags:
  package-name:
    flag-name: true
```
## Non-project config

### docker

See [Docker configuration](Docker#configuration).

### connection-count

Integer indicating how many simultaneous downloads are allowed to happen

Default: 8

### hide-th-loading

Strip out the "Loading ..." lines from GHC build output, produced when using Template Haskell

Default: true

### latest-snapshot-url

URL providing a JSON with information on the latest LTS and Nightly snapshots, used for automatic project configuration.

Default: https://www.stackage.org/download/snapshots.json

### package-indices

### system-ghc

### install-ghc

### require-stack-version

Require a version of stack within the specified range
([cabal-style](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information))
to be used for this project. Example: `require-stack-version: "== 0.1.*"`

Default: "-any"

### arch/os

Set the architecture and operating system for GHC, build directories, etc. Values are those recognized by Cabal, e.g.:

    arch: i386, x86_64
    os: windows, linux

You likely only ever want to change the arch value. This can also be set via the command line.
