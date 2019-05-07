<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Azure CI

This page documents how to use Stack on [Azure
CI](http://dev.azure.com/).

## Quick Start

Note that you have to create [azure
pipelines](#creating-azure-pipelines) for your project and then you
need to put the relevant configuration files:

* For simple Azure configuration, copy-paste the
  [azure-simple](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-simple.yml)
  file into `azure-pipelines.yml`.
* For complex Azure configuration, you need to take the below linked
  four files and put all of them into the `.azure` directory except
  the `azure-pipelines.yml` file which should be put in the root of
  the repository.

For a more detailed explanation, you can read further.

## Simple and Complex configuration

We provide two fully baked configuration ready to be
used on your projects:

* [The simple Azure configuration](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-simple.yml)
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
  - [azure-pipelines.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-pipelines.yml) : This is the starter file used by the Azure CI.
  - [azure-linux-template.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-linux-template.yml) : Template for Azure Linux build
  - [azure-osx-template.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-osx-template.yml) : Template for Azure macOS build
  - [azure-windows-template.yml](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-windows-template.yml) : Template for Azure Windows build

  __NOTE__: It is likely going to be necessary to modify this configuration to
  match the needs of your project, such as tweaking the build matrix to alter
  which GHC versions you test against, or to specify GHC-version-specific
  `stack.yaml` files if necessary. Don't be surprised if it doesn't work the
  first time around. See the multiple GHC section below for more information.

## Creating Azure Pipelines

Each of these configurations is ready to be used immediately. But
before we go into where to put them, we have to create pipeline for
your project in Azure CI platform:

* Go to [dev.azure.com](https://dev.azure.com). You have to initially
  sign-in to your microsoft account there.
* Once you have logged in to your Microsoft account, you have to sign
  in to [Azure
  devops](https://user-images.githubusercontent.com/737477/52465678-70963080-2ba5-11e9-83d8-84112b140236.png)
  from there.
* You will be [greeted with a
  dashboard](https://user-images.githubusercontent.com/737477/52465677-70963080-2ba5-11e9-904a-c15c7c0524ef.png)
  where you can create your projects.
* Click the "Create Project" button and fill the [relevant information
  in the dialog](https://user-images.githubusercontent.com/737477/52465676-70963080-2ba5-11e9-82a4-093ee58f11c9.png) and then click the "Create" button.
* This will lead you to the [project
  dashboard](https://user-images.githubusercontent.com/737477/52465675-6ffd9a00-2ba5-11e9-917e-3dec251fcc87.png)
  page where you can create pipelines.
* Click on "Pipelines" in the left menu. This will load the [pipelines
  page](https://user-images.githubusercontent.com/737477/52465673-6ffd9a00-2ba5-11e9-97a4-04e703ae1fbc.png)
  on the right.
* Click on the button "New Pipeline" and you have to follow through
  the wizard there. You need to choose your github repository (or
  Azure repos) and follow the wizard. Note that in the [Configure
  step](https://user-images.githubusercontent.com/737477/52465670-6ffd9a00-2ba5-11e9-83a3-9fffdacbf249.png)
  you have to select the "Starter Pipeline". This will open up an
  [editor
  window](https://user-images.githubusercontent.com/737477/52465669-6f650380-2ba5-11e9-9662-e9c6fc2682b5.png). You
  can leave the existing yaml configuration there as it is and click
  the "Save and run" button.  That will popup a
  [dialog](https://user-images.githubusercontent.com/737477/52465668-6f650380-2ba5-11e9-9203-6347a609e3c4.png). Select
  the relevant option and click "Save and run" button. (Note that this
  step would have created `azure-pipelines.yml` in your repository,
  you have replace that with the appropriate configuration file.)

The rest of this document explains the details of common Azure
configurations for those of you who want to tweak the above
configuration files or write your own.

*Note:* both Azure and Stack infrastructures are actively
 developed. We try to document best practices at the moment.

## Infrastructure

Note that you need at least one agent to build your code. You can
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
script](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/azure/azure-simple.yml)
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
    cabal-8.4.4:
      BUILD: cabal
      GHCVER: 8.4.4
      CABALVER: 2.4
    cabal-8.6.5:
      BUILD: cabal
      GHCVER: 8.6.5
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
