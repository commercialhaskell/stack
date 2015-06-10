This page is intended to fully document all configuration options available in the stack.yaml file. Note that, as we're still in beta, this page is likely to be both *incomplete* and sometimes *inaccurate*. If you see such cases, please update the page, and if you're not sure how, open an issue labeled "question".

The stack.yaml configuration options break down into project specific and non-project specific options. The latter can be specified in your global config (/etc/stack/config) and user config (~/.stack/stack.yaml), as well as in the project config. The former must be in the project config.

## Project config

### packages

### extra-deps

### resolver

### flags

## Non-project config

### docker

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