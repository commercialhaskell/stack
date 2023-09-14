<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Dev Containers

Stack's Dev Containers provide the following tools:

1. The [**Haskell Toolchain**](https://www.haskell.org/ghcup/install/#supported-tools):  
   (installed at `/usr/local/bin`[^1])
    * [GHC](https://www.haskell.org/ghc)
    * [Cabal](https://cabal.readthedocs.io)
    * Stack
    * [HLS](https://haskell-language-server.readthedocs.io) (Default Dev Container only)
1. Additionally:
    * [Git](https://git-scm.com)
    * [HLint](https://hackage.haskell.org/package/hlint)
    * [yamllint](https://yamllint.readthedocs.io)
    * [ShellCheck](https://www.shellcheck.net)
    * [hadolint](https://github.com/hadolint/hadolint)

[^1]: `PATH=$HOME/.cabal/bin:$HOME/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin`

!!! info

    Executables installed with Cabal (at `$HOME/.cabal/bin`) or Stack/Pip (at
    `$HOME/.local/bin`) take precedence over the same executable installed at
    `/usr/local/sbin`, `/usr/local/bin`, etc.

[VS Code](https://code.visualstudio.com) is used as IDE, with the following
extensions pre‑installed:

* [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
  (Default Dev Container only)
* [GitHub Pull Requests and Issues](https://marketplace.visualstudio.com/items?itemName=GitHub.vscode-pull-request-github)
* [GitLens — Git supercharged](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens)
    * Pinned to version 11.7.0 due to unsolicited AI content
  in recent versions
* [Git Graph](https://marketplace.visualstudio.com/items?itemName=mhutchie.git-graph)
* [ShellCheck](https://marketplace.visualstudio.com/items?itemName=timonwong.shellcheck)
* [hadolint](https://marketplace.visualstudio.com/items?itemName=exiasr.hadolint)
* [Resource Monitor](https://marketplace.visualstudio.com/items?itemName=mutantdino.resourcemonitor)

## Parent images

These Dev Containers are derived from the same docker images used to build the
*statically linked* Linux amd64 and arm64 binary releases of Stack itself.

Those multi‑arch (`linux/amd64`, `linux/arm64/v8`) *ghc‑musl* images are based
on Alpine Linux (i.e. [musl libc](https://musl.libc.org) and
[BusyBox](https://www.busybox.net)) and contain *unofficial* builds of GHC.

To make sure only the GHC available in the Dev Containers is used, flags
<nobr>`--system-ghc`</nobr> and <nobr>`--no-install-ghc`</nobr> are set
system‑wide in `/etc/stack/config.yaml`. Reason:

1. the *official* GHC bindists for Alpine Linux (`x86_64`) are just too buggy.
2. there are currently (2023‑09‑05) no bindists for Alpine Linux (`AArch64`).

## Usage

For local/remote usage with VS Code, please follow the instructions at
[Developing inside a Container](https://code.visualstudio.com/docs/devcontainers/containers).

For use with Github Codespaces, please follow the instruction at
[Creating a codespace for a repository](https://docs.github.com/en/codespaces/developing-in-codespaces/creating-a-codespace-for-a-repository#creating-a-codespace-for-a-repository).

### Persistence

Data in the following locations is persisted:

1. The user's home directory (`/home/vscode`)[^2]
2. The Dev Container's workspace (`/workspaces`)

[^2]: Alternatively for the root user (`/root`). Use with Docker/Podman in
*rootless mode*.

This is accomplished either via a *volume* or *bind mount* (or *loop device*
on Codespaces) and is preconfigured.

!!! info

    **Codespaces: A 'Full Rebuild Container' resets the home directory.**  
    This is never necessary unless you want exactly that.

## Build Stack

### Using cabal

!!! info

    Default Dev Container only.

Command <nobr>`cabal build`</nobr> to build the `stack` executable.

Append <nobr>`--flag=static`</nobr> to build a *statically linked* `stack`
executable that can run on any Linux machine of the same architecture.

### Using Stack

Command <nobr>`stack build`</nobr> to build the `stack` executable.

Append <nobr>`--flag=stack:static`</nobr> to build a *statically linked*
`stack` executable that can run on any Linux machine of the same architecture.

Append <nobr>`--stack-yaml stack-ghc-$GHC_VERSION.yaml`</nobr> if you want to
use an experimental project‑level configuration with the appropriate Dev
Container.

## Haskell Language Server (HLS)

The
[Haskell Language Server](https://github.com/haskell/haskell-language-server)
and the
[Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
are only available in the default Dev Container. In order to use the Haskell
extension, you must first configure the project for the build tool of your
choice.

<!--
**Stack**

Place the cradle ([hie.yaml](assets/cradles/stack/hie.yaml)) for Stack in the
root of the workspace: `cp -f .devcontainer/assets/cradles/stack/hie.yaml .`

**Cabal**

Place the cradle ([hie.yaml](assets/cradles/cabal/hie.yaml)) for Cabal in the
root of the workspace: `cp -f .devcontainer/assets/cradles/cabal/hie.yaml .`
-->

See [Stack's code (advanced) > Contributors > Contributor's guide: Haskell Language Server](../../CONTRIBUTING/#haskell-language-server)
for cradles (`hie.yaml` files) that should suffice to configure the HLS
explicitly for `./Setup.hs` and each of the buildable components in Stack's
Cabal file.

### Haskell extension

Choose `Manually via PATH` when asked the following question:

<img width="520" alt="Manage HLS" src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/manageHLS.png">

## Issues

If there is a problem with a Dev Container, please
[open an issue](https://github.com/benz0li/ghc-musl/issues/new) at its
[parent images](#parent-images)' repository at
[https://github.com/benz0li/ghc-musl](https://github.com/benz0li/ghc-musl).
