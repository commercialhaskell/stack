<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Self-hosted runners

[GitHub Actions](https://docs.github.com/en/actions) is used to do CI on Stack.

In the case of the Linux/AArch64 platform, since 8 February 2025, that is done
with a GitHub-hosted runner (currently in public preview). Previously, that was
done in a separate `linux-arm64` job of the `integration-tests.yml` workflow
that ran on a self-hosted runner for Linux and ARM64. Given that the
GitHub-hosted runner is only in preview, the documentation of the self-hosted
runner is being preserved for the time being.

## Introduction

The current basic setup was:

* FP Complete has an Oracle Cloud account that provides a free tier that
  includes a really powerful ARM64 machine;
* within Oracle Cloud, FP Complete are running an Ubuntu/ARM64 instance; and
* on that instance, FP Complete are running the GitHub Runner software.

The runner name is `stack-github-action3` and the machine name is
`stack-github-action3`.

Occasionally Oracle will turn off the machine because:

* Oracle thinks it is not being used (because of the free tier); and/or
* other things, like disk space filling up.

The disk space can fill up due to obsolete Docker images, versions of GHC
installed by Stack or obsolete snapshots created by Stack.

## Managing the `stack-github-action3` runner

With the appropriate authority installed on the server, a maintainer can manage
the runner remotely using SSH, with command
`ssh ubuntu@arm-runner.stackage.org`.

!!! info

    The authority is the addition of the maintainer's SSH public key to the end
    of file `~/.ssh/authorized_keys` on the server.

This is best done using [`tmux`](https://github.com/tmux/tmux/wiki), a terminal
multiplexer, as follows:
~~~sh
$ # In a shell, command tmux to create a new session with a single window with a
$ # single pane (a pseudo terminal). The session will be displayed on the screen
$ # by a client:
$ tmux new-session
$ # Send the following command to that pseudo terminal, to connect to the
$ # remote host:
$ ssh ubuntu@arm-runner.stackage.org
$ # In the remote host, change to the actions-runner directory:
$ cd actions-runner
$ # In the remote host, start the runner:
$ ./run.sh
$ # Detach the current client from the session by the key combination of
$ # 'C-b' 'd' (where 'C-b' is CTRL+b). The session will continue to run in the
$ # background:
$ C-b d
~~~

The available disk space can be queried with command `df -h`; the relevant entry
is for filesystem `/dev/sda1`.

If the available space is low, that may be due to unncessary GHC versions
installed in Stack's `programs` directory.

## Re-configuring a removed `stack-github-action3` runner

A self-hosted runner is automatically removed from GitHub if it has not
connected to GitHub Actions for more than 14 days.

Re-adding a self-hosted runner to GitHub requires it to be configured, and
GitHub will suggest a configuration command like:

~~~text
./config.sh --url https://github.com/commercialhaskell/stack --token <token_value>
~~~

However, to re-use and configure the runner machine, you have first to delete
the `.runner` file inside the `actions-runner` directory. This allows the runner
to be registered without having to re-download the self-hosted runner
application.

## The `ghc-arm-5` runner

From 9 February 2024, the Haskell Foundation sought to provide an alternative
runner named `ghc-arm-5` but that was based on NixOS and proved to be
incompatible.

## Alternatives to the self-hosted runners

One alternative to the self-hosted runners is to build statically-linked Stack
executables for Linux/AArch64 on macOS/AArch64. This can be done thanks to
the multi-architecture Docker images built and published by Olivier Benz, at
https://gitlab.com/benz0li/ghc-musl.

GitHub provides a GitHub-hosted macOS/AArch64 runner (`macOS-14`).
Unfortunately, that is macOS/M1 and the M1 machine architecture does not support
nested virtualisation. This rules out using Docker, as the runner is itself a
virtual machine.

However, this solution can be applied locally and the build outputs for the
Linux/AArch64 platform added manually to the result of the GitHub workflow.

The experimental Haskell script `etc/scripts/release-linux-aarch64.hs` is
intended to facilitate the building of statically-linked Stack executables for
Linux/AArch64 on macOS/AArch64.
