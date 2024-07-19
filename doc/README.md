<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The Haskell Tool Stack

Welcome to the [Haskell](https://www.haskell.org/) programming language and the
Haskell Tool Stack (Stack)! Stack is a program for developing Haskell projects.
It is aimed at new and experienced users of Haskell and seeks to support
them fully on Linux, macOS and Windows.

Haskell code is compiled by the
[Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC), which can also
be used interactively.

<img src="https://i.imgur.com/WW69oTj.gif" width="50%" align="right">

Stack features include:

* Installing GHC automatically.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.
* Using GHC interactively.

## How to install Stack

Stack can be installed on most Unix-like operating systems (including macOS) and
Windows. It will require at least about 5 GB of disk space, for use with one
version of GHC.

Stack can be installed directly or by using the GHCup tool.

=== "Directly"

    Stack can be installed directly on various operating systems.

    === "Linux"

        For most Linux distributions, on x86_64 or AArch64 machine
        architectures, the easiest way to install Stack is to command either:

        ~~~text
        curl -sSL https://get.haskellstack.org/ | sh
        ~~~

        or:

        ~~~text
        wget -qO- https://get.haskellstack.org/ | sh
        ~~~

        These commands download a script file and run it using `sh`.

        !!! note

            The script at [get.haskellstack.org](https://get.haskellstack.org/)
            will ask for root access using `sudo`. It needs such access in order
            to use your platform's package manager to install dependencies and
            to install to `/usr/local/bin`. If you prefer more control, follow
            the manual installation instructions in the
            [install and upgrade guide](install_and_upgrade.md).

    === "macOS"

        From late 2020, Apple began a transition from Mac computers with Intel
        processors (Intel-based Mac) to
        [Mac computers with Apple silicon](https://support.apple.com/en-gb/HT211814).

        === "Intel-based"

            For most Intel-based Mac computers, the easiest way to install Stack
            is to command either:

            ~~~text
            curl -sSL https://get.haskellstack.org/ | sh
            ~~~

            or:

            ~~~text
            wget -qO- https://get.haskellstack.org/ | sh
            ~~~

            These commands download a script file and run it using `sh`.

            !!! note

                The script at
                [get.haskellstack.org](https://get.haskellstack.org/) will ask
                for root access using `sudo`. It needs such access in order to
                use your platform's package manager to install dependencies and
                to install to `/usr/local/bin`. If you prefer more control,
                follow the manual installation instructions in the
                [install and upgrade guide](install_and_upgrade.md).

        === "Apple silicon"

            Mac computers with Apple silicon have an M series chip. These chips
            use an architecture known as ARM64 or AArch64.

            For Mac computers with Apple silicon, the easiest way to install
            Stack is to command either:

            ~~~text
            curl -sSL https://get.haskellstack.org/ | sh
            ~~~

            or:

            ~~~text
            wget -qO- https://get.haskellstack.org/ | sh
            ~~~

            These commands download a script file and run it using `sh`.

            !!! note

                The script at
                [get.haskellstack.org](https://get.haskellstack.org/) will ask
                for root access using `sudo`. It needs such access in order to
                use your platform's package manager to install dependencies and
                to install to `/usr/local/bin`. If you prefer more control,
                follow the manual installation instructions in the
                [install and upgrade guide](install_and_upgrade.md).

    === "Windows"

        Most machines using the Windows operating system have a x86_64
        architecture. More recently, Microsoft has provided Windows on Arm that
        runs on other processors.

        === "x86_64"

            On 64-bit Windows, the easiest way to install Stack is to download
            and install the
            [Windows installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

            !!! info

                By default, the Windows installer will set the
                [Stack root](stack_root.md) to `C:\sr`.

            !!! note

                Systems with antivirus software may need to add Stack to the
                list of 'trusted' applications.

        === "Windows on Arm"

            The GHC project does not yet provide a version of GHC that runs on
            Windows on Arm.

    === "Other/direct downloads"

        For other operating systems and direct downloads see the
        [install and upgrade guide](install_and_upgrade.md).

=== GHCup

    The separate [GHCup](https://www.haskell.org/ghcup/) project provides a tool
    that can be used to install Stack and other Haskell-related tools, including
    GHC and
    [Haskell Language Server](https://github.com/haskell/haskell-language-server)(HLS).
    HLS is a program that is used by Haskell extensions for popular code
    editors.

    GHCup provides Stack for some combinations of machine architecture and
    operating system not provided elsewhere.

    By default, the script to install GHCup (which can be run more than once)
    also configures Stack so that if Stack needs a version of GHC, GHCup takes
    over obtaining and installing that version.

## How to upgrade Stack

If Stack is already installed, upgrading it depends on whether you are using
Stack or GHCup to manage versions of Stack.

=== "Stack"

    If Stack is already installed, you can upgrade it to the latest version by
    the command:

    ~~~text
    stack upgrade
    ~~~

    !!! warning

        If you used [GHCup](https://www.haskell.org/ghcup/) to install Stack,
        you should also use GHCup, and not Stack, to upgrade Stack.

=== "GHCup"

    The separate [GHCup](https://www.haskell.org/ghcup/) project provides
    guidance about how to use GHCup to manage versions of tools such as Stack.

## Quick Start guide

For an immediate experience of using Stack to build an executable with Haskell,
first you need to follow the [guide to install Stack](#how-to-install-Stack).

### Step 1: Start your new project

A complex project can have more than one package and each package can have more
than one executable (program). However, to start a new single-package project
named `my-project`, issue these four commands in a terminal:

1.  `stack new my-project`

    This command will create a new directory, named `my-project`. It contains
    all the files needed to start a project correctly, using a default template.

2.  `cd my-project`

    This command will change the current working directory to that directory.

3.  `stack build`

    This command will build the template project and create an executable named
    `my-project-exe` (on Windows, `my-project-exe.exe`).

    First, if necessary, Stack will download a version of GHC in an isolated
    location. That won't interfere with other GHC installations on your system.
    (On Windows, if necessary, Stack will also download
    [MSYS2](https://www.msys2.org/). MSYS2 is a project that provides popular
    tools for developers on Windows).

4.  `stack exec my-project-exe`

    This command will run (execute) the built executable, in Stack's
    environment.

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

Stack has a strong focus on plans for building that are reproducible; projects
that have more than one package; and a consistent, easy-to-learn set of
Stack commands. It also aims to provide the ability to customise and power that
experienced developers need.

Stack does not stand alone. It is built on the great work provided by:

*   The __Glasgow Haskell Compiler__ ([GHC](https://www.haskell.org/ghc/)), the
    premier Haskell compiler. Stack will manage your GHC installations and
    automatically select the appropriate version of GHC for your project.
*   The __Cabal build system__. Cabal is a specification for defining Haskell
    packages and a [library](https://hackage.haskell.org/package/Cabal) for
    performing builds.

    !!! info

        Cabal is also the name of another tool used for building Haskell code,
        provided by the `cabal-install` package. This guide distinguishes
        between them by Cabal (the library) and Cabal (the tool).

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
originally spearheaded by [FP Complete](https://www.fpcomplete.com/) to answer
the needs of commercial Haskell users. It has since become a thriving open
source project meeting the needs of Haskell users of all types.

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

A [guide](CONTRIBUTING.md) is provided to help potential contributors to the
Stack project.

If you have already installed a version of Stack and the
[Git application](https://git-scm.com/) the followings steps should get you
started with building Stack from source with Stack:

1.  Clone the `stack` repository from GitHub with the command:

    ~~~text
    git clone https://github.com/commercialhaskell/stack.git
    ~~~

2.  Change the current working directory to the cloned `stack` directory with
    the command:

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

The `stack uninstall` command provides information about how to uninstall Stack.

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
that you have built using Stack.
