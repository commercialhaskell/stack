<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Dev Containers

A *container* is an isolated area of memory where application software and some
drivers execute. A [Development Container](https://containers.dev) (or Dev
Container for short) allows a container to be used as a full‑featured
development environment.

Stack provides the following Dev Containers:

* a default one, intended for use with Stack's default project‑level
  configuration file (`stack.yaml`); and
* alternative ones, intended for use with Stack's experimental project‑level
  configuration files (in anticipation of building Stack with more recent
  versions of GHC).

Stack's Dev Containers provide the following tools:

1. The
   [Haskell Toolchain](https://www.haskell.org/ghcup/install/#supported-tools)
   ([GHC](https://www.haskell.org/ghc), Stack,
   [Cabal (the tool)](https://cabal.readthedocs.io) and (in the default Dev
   Container only) [HLS](https://haskell-language-server.readthedocs.io))
2. [Git](https://git-scm.com)
3. [HLint](https://hackage.haskell.org/package/hlint)
4. [yamllint](https://yamllint.readthedocs.io)
5. [ShellCheck](https://www.shellcheck.net)
6. [hadolint](https://github.com/hadolint/hadolint)

The tools in the Haskell Toolchain are installed at `/usr/local/bin`.

!!! info

    The PATH is
    `$HOME/.cabal/bin:$HOME/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin`.
    Consequently, executables installed with Cabal (the tool) (at
    `$HOME/.cabal/bin` or `$HOME/.local/bin`) or Stack or Pip (at
    `$HOME/.local/bin`) take precedence over the same executable installed at
    `/usr/local/sbin`, `/usr/local/bin`, etc.

[Visual Studio Code](https://code.visualstudio.com) (VS Code) is used as IDE,
with the following extensions pre‑installed:

*   [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
    (Default Dev Container only)
*   [GitHub Pull Requests and Issues](https://marketplace.visualstudio.com/items?itemName=GitHub.vscode-pull-request-github)
*   [GitLens — Git supercharged](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens)
    *   Pinned to version 11.7.0 due to unsolicited AI content in subsequent
        versions
*   [Git Graph](https://marketplace.visualstudio.com/items?itemName=mhutchie.git-graph)
*   [ShellCheck](https://marketplace.visualstudio.com/items?itemName=timonwong.shellcheck)
*   [hadolint](https://marketplace.visualstudio.com/items?itemName=exiasr.hadolint)
*   [Resource Monitor](https://marketplace.visualstudio.com/items?itemName=mutantdino.resourcemonitor)

## Parent images

Stack's Dev Containers are derived from Docker images that are used to build
the *statically linked* Linux/x86_64 and Linux/AArch64 binary distributions of
Stack. The repository providing those images is identified in Stack's
project-level configuration files under the
[`docker` key](topics/docker_integration.md#configuration). For example:

~~~yaml
docker:
  enable: false
  repo: quay.io/benz0li/ghc-musl:9.10.3
~~~

These Docker images are multi‑architecture (`linux/amd64`, `linux/arm64/v8`)
<nobr>*GHC musl*</nobr> images. They are based on Alpine Linux (that is
[musl libc](https://musl.libc.org) and [BusyBox](https://www.busybox.net)).

The images contain *unofficial* and *untested* binary distributions of GHC (that
is, ones not released by the GHC developers). That is because the official GHC
binary distributions for Alpine Linux/x86_64 have known bugs
([GHC issue 23043](https://gitlab.haskell.org/ghc/ghc/-/issues/23043) and
[GHC issue 25093](https://gitlab.haskell.org/ghc/ghc/-/issues/25093)).

Stack's global configuration (`/etc/stack/config.yaml`) sets
<nobr>`system-ghc: true`</nobr> and <nobr>`install-ghc: false`</nobr>. That
ensures that only the GHC available in the Dev Containers is used.

## Usage

You can run Dev Containers locally/remotely with VS Code, or create a
[GitHub Codespace](https://github.com/features/codespaces) for a branch in a
repository to develop online.

=== "VS Code"

    Follow the instructions at
    [Developing inside a Container](https://code.visualstudio.com/docs/devcontainers/containers).

=== "GitHub Codespaces"

    For use with GitHub Codespaces, follow the instructions at
    [Creating a codespace for a repository](https://docs.github.com/en/codespaces/developing-in-codespaces/creating-a-codespace-for-a-repository#creating-a-codespace-for-a-repository).

## Build Stack

Stack can be built with Stack (which is recommended) or with Cabal (the tool).

=== "Stack"

    Command <nobr>`stack build`</nobr> to build the `stack` executable.

    Append <nobr>`--flag=stack:static`</nobr> to build a *statically linked*
    `stack` executable that can run on any Linux machine of the same
    architecture.

    Append <nobr>`--stack-yaml stack-ghc-$GHC_VERSION.yaml`</nobr> if you want
    to use an experimental project‑level configuration with the appropriate Dev
    Container.

=== "Cabal (the tool)"

    !!! info

        Default Dev Container only.

    Command <nobr>`cabal build`</nobr> to build the `stack` executable.

    Append <nobr>`--flag=static`</nobr> to build a *statically linked* `stack`
    executable that can run on any Linux machine of the same architecture.

## Haskell Language Server (HLS)

The
[Haskell Language Server](https://github.com/haskell/haskell-language-server)
and the
[Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
are only available in the default Dev Container. In order to use the Haskell
extension, you must first configure the project for the tool used for building
of your choice.

<!--
**Stack**

Place the cradle ([hie.yaml](assets/cradles/stack/hie.yaml)) for Stack in the
root of the workspace: `cp -f .devcontainer/assets/cradles/stack/hie.yaml .`

**Cabal**

Place the cradle ([hie.yaml](assets/cradles/cabal/hie.yaml)) for Cabal in the
root of the workspace: `cp -f .devcontainer/assets/cradles/cabal/hie.yaml .`
-->

See the documentation at
[Contributing: Haskell Language Server](CONTRIBUTING.md#haskell-language-server)
for cradles (`hie.yaml` files) that should suffice to configure the HLS
explicitly for `./Setup.hs` and each of the buildable components in Stack's
Cabal file.

### Haskell extension

Choose `Manually via PATH` when asked the following question:

<img width="520" alt="Manage HLS" src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack@master/doc/img/manageHLS.png">

## Issues

If there is a problem with a Dev Container, please
[open an issue](https://github.com/benz0li/ghc-musl/issues/new) at its
[parent images](#parent-images)' repository at
[https://github.com/benz0li/ghc-musl](https://github.com/benz0li/ghc-musl).
