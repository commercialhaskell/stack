<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The Haskell Tool Stack

Welcome to the [Haskell](https://www.haskell.org/) programming language and the
Haskell Tool Stack (Stack)! Stack is a program for developing Haskell projects.
It is aimed at Haskellers both new and experienced. It is cross-platform and
aims to support fully users on Linux, macOS and Windows.

<img src="https://i.imgur.com/WW69oTj.gif" width="50%" align="right">

Stack features:

* Installing the [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/)
  automatically, in an isolated location.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.

## How to install Stack

Stack can be installed on most Unix-like operating systems (including macOS) and
Windows.

!!! info

    In addition to the methods described below, Stack can also be installed
    using the separate [GHCup](https://www.haskell.org/ghcup/) installer for
    Haskell-related tools. GHCup provides Stack for some combinations of machine
    architecture and operating system not provided elsewhere. By default, the
    script to install GHCup (which can be run more than once) also configures
    Stack so that if Stack needs a version of GHC, GHCup takes over obtaining
    and installing that version.

=== "Linux"

    For most Linux distributions, the easiest way to install Stack
    directly is to command:

    ~~~text
    curl -sSL https://get.haskellstack.org/ | sh
    ~~~

    or:

    ~~~text
    wget -qO- https://get.haskellstack.org/ | sh
    ~~~

    !!! note

        The script at [get.haskellstack.org](https://get.haskellstack.org/) will
        ask for root access using `sudo`. It needs such access in order to use
        your platform's package manager to install dependencies and to install
        to `/usr/local/bin`. If you prefer more control, follow the manual
        installation instructions in the
        [install and upgrade guide](install_and_upgrade.md).

=== "macOS"

    From late 2020, Apple began a transition from Mac computers with Intel
    processors (Intel-based Mac) to
    [Mac computers with Apple silicon](https://support.apple.com/en-gb/HT211814).

    === "Intel-based"

        For most Intel-based Mac computers, the easiest way to install Stack
        directly is to command:

        ~~~text
        curl -sSL https://get.haskellstack.org/ | sh
        ~~~

        or:

        ~~~text
        wget -qO- https://get.haskellstack.org/ | sh
        ~~~

        !!! note

            The script at [get.haskellstack.org](https://get.haskellstack.org/)
            will ask for root access using `sudo`. It needs such access in order
            to use your platform's package manager to install dependencies and
            to install to `/usr/local/bin`. If you prefer more control, follow
            the manual installation instructions in the
            [install and upgrade guide](install_and_upgrade.md).

    === "Apple silicon"

        Mac computers with Apple silicon have an M1, M1 Pro, M1 Max, M1 Ultra or
        M2 chip. These chips use an architecture known as ARM64 or AArch64.

        For Mac computers with Apple silicon, the easiest way to install Stack
        directly is to obtain the 'unofficial' `osx-aarch64` binary distribution
        released by the GHCup developers and copy it to a location on the PATH.
        `*.tar.gz` archive files containing those binary distributions are
        available at the directories here:
        [:material-cloud-download-outline:](https://downloads.haskell.org/ghcup/unofficial-bindists/stack/).

        It is still possible to use the commands:

        ~~~text
        curl -sSL https://get.haskellstack.org/ | sh
        ~~~

        or:

        ~~~text
        wget -qO- https://get.haskellstack.org/ | sh
        ~~~

        However, those commands will download and install the version of Stack
        for Intel-based Mac computers. Mac computers with Apple silicon will
        use Apple's
        [Rosetta 2 application](https://support.apple.com/en-gb/HT211861) to
        use that version of Stack.

        Apple's Terminal application will not detect automatically that Rosetta
        has not yet been installed. Rosetta can be manually installed by
        commanding:

        ~~~text
        softwareupdate --install-rosetta
        ~~~

=== "Windows"

    On 64-bit Windows, the easiest way to install Stack directly is to download
    and install the
    [Windows installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

    !!! note

        Systems with antivirus software may need to add Stack to the list of
        'trusted' applications.

=== "Other/direct downloads"

    For other operating systems and direct downloads, see the
    [install and upgrade guide](install_and_upgrade.md).

## How to upgrade Stack

If Stack is already installed, you can upgrade it to the latest version by the
command:

~~~text
stack upgrade
~~~

!!! note

    If you used [GHCup](https://www.haskell.org/ghcup/) to install Stack, you
    should also use GHCup, and not Stack, to upgrade Stack.

## Quick Start guide

For an immediate experience of using Stack to build an executable with Haskell,
first you need to follow the [guide to install Stack](#how-to-install-Stack).

### Step 1: Start your new project

To start a new project named `my-project`, issue these four commands in a
terminal:

~~~text
stack new my-project
cd my-project
stack build
stack exec my-project-exe
~~~

- The `stack new my-project` command will create a new directory, named
  `my-project`. It contains all the files needed to start a project correctly,
  using a default template.
- The `cd my-project` command will change the current working directory to that
  directory.
- The `stack build` command will build the template project and create an
  executable named `my-project-exe` (on Windows, `my-project-exe.exe`). First,
  if necessary, Stack will download a version of GHC in an isolated location.
  That won't interfere with other GHC installations on your system.
- The `stack exec my-project-exe` command will run (execute) the built
  executable, in Stack's environment.

For a complete list of Stack's commands, and flags and options common to those
commands, simply command:

~~~text
stack
~~~

For help on a particular Stack command, including flags and options specific to
that command, for example `stack build`, command:

~~~text
stack build --help
~~~

If you want to launch a run-eval-print loop (REPL) environment, then command:

~~~text
stack repl
~~~

!!! info

    `stack ghci` can be used instead of `stack repl`. GHCi is GHC's REPL tool.

People organise Haskell code into packages. If you want to use Stack to install
an executable provided by a Haskell package, then all you have to do is command:

~~~text
stack install <package-name>
~~~

### Step 2: Next steps

The `stack new my-project` command in step one should have created the following
files and directories (among others):

~~~text
.
├── app
│   └── Main.hs
├── src
│   └── Lib.hs
├── test
│   └── Spec.hs
├── my-project.cabal
├── package.yaml
└── stack.yaml
~~~

The Haskell source code for the executable (application) is in file `Main.hs`.

The executable uses a library. Its source code is in file `Lib.hs`.

The contents of `my-project.cabal` describes the project's package. That file is
generated by the contents of `package.yaml`.

!!! info

    If you want, you can delete the `package.yaml` file and update the
    `my-project.cabal` file directly. Stack will then use that file.

The contents of `stack.yaml` describe Stack's own project-level configuration.

You can edit the source files in the `src` directory (used for the library) or
the `app` directory (used for the executable (application)).

As your project develops, you may need to depend on a library provided by
another Haskell package. If you do, then add the name of that new package to the
file `package.yaml`, in its `dependencies:` section.

!!! info

    When you use `stack build` again, Stack will use `package.yaml` to create an
    updated `my-project.cabal` for you.

If Stack reports that the Stack configuration has no specified version for the
new package, then follow Stack's likely recommended action to add a specific
version of that package your project's `stack.yaml` file, in its `extra-deps:`
section.

That was a really fast introduction on how to start to code in Haskell using
Stack. If you want to go further, we highly recommend you read Stack's
introductory [user's guide](GUIDE.md).

## Complete guide to Stack

A complete [user's guide](GUIDE.md) to Stack is available, covering all of
the most common ways to use Stack. Terms used in Stack's documentation are also
explained in the [glossary](glossary.md).

## Why Stack?

Stack is a build tool for Haskell designed to answer the needs of Haskell users,
both new and experienced. It has a strong focus on reproducible build plans,
multi-package projects, and a consistent, easy-to-learn set of Stack commands.
It also aims to provide the customizability and power that experienced
developers need.

Stack does not stand alone. It is built on the great work provided by:

* The __Glasgow Haskell Compiler__ ([GHC](https://www.haskell.org/ghc/)), the
  premier Haskell compiler. Stack will manage your GHC installations and
  automatically select the appropriate version of GHC for your project.
* The __Cabal build system__. Cabal is a specification for defining Haskell
  packages and a [library](https://hackage.haskell.org/package/Cabal) for
  performing builds.

    !!! info

        Cabal is also the name of another build tool, provided by the
        `cabal-install` package. This guide distinguishes between them by Cabal
        (the library) and Cabal (the tool).

* The __Hackage Haskell Package Repository__, a
  [repository](https://hackage.haskell.org/) of Haskell packages providing
  thousands of open source libraries and applications to help you get your work
  done.
* The __Stackage package collection__, sets of packages from Hackage that are
  [curated](https://www.stackage.org/). That is, they are regularly tested for
  compatibility. Stack defaults to using Stackage package sets to avoid
  problems with incompatible dependencies.

Stack is provided by a team of volunteers and companies under the auspices of
the [Commercial Haskell](http://commercialhaskell.com/) group. The project was
spearheaded by [FP Complete](https://www.fpcomplete.com/) to answer the needs of
commercial Haskell users. It has since become a thriving open source project
meeting the needs of Haskell users of all stripes.

If you'd like to get involved with Stack, check out the
[newcomer friendly](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3a%22newcomer+friendly%22)
label on the GitHub issue tracker.

## Questions, feedback, and discussion

* For answers to frequently asked questions about Stack, please see the
  [FAQ](faq.md).
* For general questions, comments, feedback and support, please post to the
  [Haskell Community](https://discourse.haskell.org/about).
* For bugs, issues, or requests, please
  [open an issue](https://github.com/commercialhaskell/stack/issues/new).
* When using Stack Overflow, please use the
  [haskell-stack](http://stackoverflow.com/questions/tagged/haskell-stack) tag.

## How to contribute to the maintenance or development of Stack

The following assumes that you already have installed a version of Stack and the
[Git application](https://git-scm.com/).

1.  Clone the `stack` repository from GitHub with the command:

    ~~~text
    git clone https://github.com/commercialhaskell/stack.git`
    ~~~

2.  Change the current working directory to the cloned `stack` directory with the
    command:

    ~~~text
    cd stack
    ~~~

3.  Build the `stack` executable using a preexisting installation of Stack with
    the command:

    ~~~text
    stack build
    ~~~

4.  Once the `stack` executable has been built, check its version with the
    command:

    ~~~text
    stack exec -- stack --version
    ~~~

    Make sure the version is the latest one.

5.  In the GitHub repository's issue tracker, look for issues tagged with
    [newcomer friendly](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3a%22newcomer+friendly%22)
    and
    [awaiting pull request](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pull+request%22)
    labels.

If you need to check your changes quickly command:

~~~text
stack repl
~~~

and then, at the REPL's prompt, command:

~~~text
:main --stack-root=<path_to_root> --stack-yaml=<path_to_stack.yaml> <COMMAND>
~~~

This allows you to set a special Stack root (instead of the default Stack root)
and to target your commands at a particular `stack.yaml` file instead of the one
found in the current directory.

## How to uninstall

To uninstall Stack, it should be sufficient to delete:

1. the Stack root directory (see `stack path --stack-root`, before you
   uninstall);
2. if different, the directory containing Stack's global YAML configuration file
   (see `stack path --global-config`, before you uninstall);
3. on Windows, the directory containing Stack's tools (see
   `stack path --programs`, before you uninstall), which is located outside of
   the Stack root directory; and
4. the `stack` executable file (see `which stack`, on Unix-like operating
   systems, or `where.exe stack`, on Windows).

You may also want to delete ``.stack-work`` directories in any Haskell projects
that you have built using Stack. The `stack uninstall` command provides
information about how to uninstall Stack.
