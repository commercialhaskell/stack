<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Azure CI

This page documents how to use Stack on [Azure
CI](http://dev.azure.com/). We assume you have basic familiarity with
Azure. We provide two fully baked example files ready to be used on
your projects:

* [The simple Azure configuration](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-simple.yml)
  is intended for applications that do not require multiple GHC
  support or cross-platform support. It builds and tests your project
  with just the settings present in your `stack.yaml` file.
* The complex Azure configuration is intended for projects that need
  to support multiple GHC versions and multiple OSes, such as open
  source libraries to be released to Hackage. It tests against
  cabal-install, as well as Stack on Linux and macOS. The
  configuration is significantly more involved to allow for all of
  this branching behavior. In the provided template, we use the Linux
  build configuration to test against various combinations of
  cabal-install and stack. The macOS and Windows configuration is used
  only for testing Stack build configuration. These are the files for
  the complex configuration:
  - [azure-pipelines.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-pipelines.yml) : This is the starter file used by the Azure CI.
  - [azure-linux-template.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-linux-template.yml) : Template for Azure Linux build
  - [azure-osx-template.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-osx-template.yml) : Template for Azure macOS build
  - [azure-windows-template.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-windows-template.yml) : Template for Azure Windows build

  __NOTE__: It is likely going to be necessary to modify this configuration to
  match the needs of your project, such as tweaking the build matrix to alter
  which GHC versions you test against, or to specify GHC-version-specific
  `stack.yaml` files if necessary. Don't be surprised if it doesn't work the
  first time around. See the multiple GHC section below for more information.

Each of these configurations is ready to be used immediately. Steps to make them work:
* For simple Azure configuration, copy-paste the
  [azure-simple](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-simple.yml)
  file into `azure-pipelines.yml` in the root of your repository.
* For complex Azure configuration, you need to take the above four
files and put them in the root of your repository.

Once you have done that, you need to create a pipeline from the Azure
Web interface to get things started.

The rest of this document explains the details of common Azure
configurations for those of you who want to tweak the above
configuration files or write your own.

*Note:* both Azure and Stack infrastructures are actively developed. We try to
 document best practices at the moment.

## Infrastructure

Note that you need atleast one agent to build your code. You can
specify which virtual image you want to choose using this configuration:

``` yaml
pool:
  vmImage: ubuntu-16.04
```

The other popular options are `macOS-10.13`, `vs2017-win2016` for Mac
and Windows respectively. You can find the [complete
list](https://docs.microsoft.com/en-us/azure/devops/pipelines/agents/hosted?view=vsts&tabs=yaml)
here.

Note that as of now, Azure CI doesn't offer any caching support. You
can use something like [cache-s3](https://github.com/fpco/cache-s3)
for explicit caching, although it may cost you. For a project with an
example usage of `cache-s3`, you can see the Azure configuration of
[stack](https://github.com/commercialhaskell/stack) repository.

## Installing Stack

Currently there is only one reasonable way to install Stack: fetch precompiled
binary from the Github.

```yaml
- script: |
    mkdir -p ~/.local/bin
    curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
  displayName: Install Stack
```

## Installing GHC

There are two ways to install GHC:

- Let Stack download GHC
- Install GHC using apt package manger. This method is only applicable
  for Debian based images.

See the [simple azure
script](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/.azure/azure-simple.yml)
for an example of the first option (letting Stack download GHC). Here,
we will show the second option:

```yaml
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER
```

For the above commands to work, you need to set the `CABALVER` and
`GHCVER` environment variable properly.

### Multiple GHC - parametrised builds

For different GHC versions, you probably want to use different
`stack.yaml` files. If you don't want to put a specific `stack.yaml`
for a particular resolver and still want to test it, you have specify
your resolver argument in `ARGS` environment variable (you will see an
example below). For cabal based builds, you have to specify both
`GHCVER` and `CABALVER` environment variables.

```
strategy:
  matrix:
    stack-def:
      BUILD: stack
      STACK_YAML: stack.yaml
    stack-lts-13:
      BUILD: stack
      STACK_YAML: stack-lts-13.yaml
    cabal-8.4.3:
      BUILD: cabal
      GHCVER: 8.4.3
      CABALVER: 2.4
    cabal-8.6.3:
      BUILD: cabal
      GHCVER: 8.6.3
      CABALVER: 2.4
    nightly:
      BUILD: stack
      ARGS: "--resolver nightly"
    style:
      BUILD: style
    pedantic:
      BUILD: pedantic
      STACK_YAML: stack.yaml
```

## Running tests

After the environment setup, actual test running is simple:

```yaml
script:
  - stack test
```

## Other details

Some Stack commands will run for long time. To avoid timeouts, use the [timeoutInMinutes](https://docs.microsoft.com/en-us/azure/devops/pipelines/process/phases?tabs=yaml&view=azdevops#timeouts) for jobs.

## Examples

- [commercialhaskell/stack](https://github.com/commercialhaskell/stack/blob/master/azure-pipelines.yml)
- [psibi/tldr-hs](http://github.com/psibi/tldr-hs)
- [psibi/wai-slack-middleware](https://github.com/psibi/wai-slack-middleware)
