<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Welcome to Stack

Welcome to the [Haskell](https://www.haskell.org/) programming language and
Stack! Stack is an established program for developing Haskell projects.[^1] It
is aimed at new and experienced users of Haskell and seeks to support them fully
on Linux, macOS and Windows.

[^1]:
    The project's first public commit was on 29 April 2015. It changed its name
    to the Haskell Tool Stack on 18 May 2015. It is now widely known simply as
    Stack.

Haskell code is compiled by the
[Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC), which can also
be used interactively.

<img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/stack-welcome.gif" width="50%" align="right">

Stack features include:

* Installing GHC automatically.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.
* Using GHC interactively.

Stack is used at the command line. You will need terminal software for your
system (which will likely come with its operating system) and a program to edit
code files. There are a number of freely-available and popular code editors that
have Haskell extensions.

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

        ??? question "Will the installation script need root access?"

            The script at [get.haskellstack.org](https://get.haskellstack.org/)
            will ask for root access using `sudo`. It needs such access in order
            to use your platform's package manager to install dependencies and
            to install to `/usr/local/bin`. If you prefer more control, follow
            the manual installation instructions in the guide to
            [setting up](install_and_upgrade.md).

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

            ??? question "Will the installation script need root access?"

                The script at
                [get.haskellstack.org](https://get.haskellstack.org/) will ask
                for root access using `sudo`. It needs such access in order
                to use your platform's package manager to install dependencies
                and to install to `/usr/local/bin`. If you prefer more control,
                follow the manual installation instructions in the guide to
                [setting up](install_and_upgrade.md).

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

            ??? question "Will the installation script need root access?"

                The script at
                [get.haskellstack.org](https://get.haskellstack.org/) will ask
                for root access using `sudo`. It needs such access in order
                to use your platform's package manager to install dependencies
                and to install to `/usr/local/bin`. If you prefer more control,
                follow the manual installation instructions in the guide to
                [setting up](install_and_upgrade.md).

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
                [Stack root](topics/stack_root.md) to `C:\sr`.

            !!! note

                Systems with antivirus software may need to add Stack to the
                list of 'trusted' applications.

            ??? warning "I have a Windows username with a space in it"

                GHC 9.4.1 and later have a bug which means they do not work if
                the path to the `ghc` executable has a space character in it.
                The default location for Stack's 'programs' directory will have
                a space in the path if the value of the `USERNAME` environment
                variable includes a space.

                A solution is to configure Stack to use a different location for
                its 'programs' directory. For further information, see the
                [`local-programs-path`](configure/yaml/non-project.md#local-programs-path)
                non-project specific configuration option documentation.

        === "Windows on Arm"

            The GHC project does not yet provide a version of GHC that runs on
            Windows on Arm.

    === "Other/direct downloads"

        For other operating systems and direct downloads see the guide to
        [setting up](install_and_upgrade.md).

=== "GHCup"

    The separate [GHCup](https://www.haskell.org/ghcup/) project provides a tool
    that can be used to install Stack and other Haskell-related tools, including
    GHC and
    [Haskell Language Server](https://github.com/haskell/haskell-language-server)
    (HLS). HLS is a program that is used by Haskell extensions for popular code
    editors.

    GHCup provides Stack for some combinations of machine architecture and
    operating system not provided elsewhere.

    By default, the script to install GHCup (which can be run more than once)
    also configures Stack so that if Stack needs a version of GHC, GHCup takes
    over obtaining and installing that version.

??? question "How do I upgrade Stack?"

    Follow the advice under [setting up](install_and_upgrade.md#upgrade-stack).

??? question "How do I remove Stack?"

    For information about how to uninstall Stack, command:

    ~~~text
    stack uninstall
    ~~~

    To uninstall Stack, it should be sufficient to delete:

    1.  the Stack root directory (see `stack path --stack-root`, before you
        uninstall);
    2.  if different, the directory containing Stack's global configuration file
        (see `stack path --global-config`, before you uninstall);
    3.  on Windows, the directory containing Stack's tools (see
        `stack path --programs`, before you uninstall), which is usually located
        outside of the Stack root directory; and
    4.  the `stack` executable file (see `which stack`, on Unix-like operating
        systems, or `where.exe stack`, on Windows).

    You may also want to delete ``.stack-work`` directories in any Haskell
    projects that you have built using Stack.

## Quick Start guide

Once Stack is installed, you can get an immediate experience of using it to
build an executable with Haskell.

### Step 1: Start your new project

A complex project can have more than one package and each package can have more
than one executable (program). However, to start a new single-package project
named `my-project`, issue these four commands in a terminal (click
:material-plus-circle: to learn more about each command):

~~~shell
stack new my-project # (1)!
cd my-project # (2)!
stack build # (3)!
stack exec my-project-exe # (4)!
~~~

1.  Create a new directory named `my-project`. It contains all the files needed
    to start a project correctly, using a default template.

2.  Change the current working directory to `my-project`.

3.  Build the template project and create an executable named `my-project-exe`.

    First, if necessary, Stack will download a version of GHC in an isolated
    location. That will not interfere with other GHC installations on your
    system. (On Windows, if necessary, Stack will also download
    [MSYS2](https://www.msys2.org/). MSYS2 is a project that provides popular
    tools for developers on Windows.)

4.  Run (execute) the built executable, in Stack's environment.

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

~~~shell
stack repl # (1)!
~~~

1.  `stack ghci` can be used instead of `stack repl`. GHCi is GHC's REPL tool.

People organise Haskell code into packages. If you want to use Stack to install
an executable provided by a Haskell package, then all you have to do is command:

~~~text
stack install <package-name>
~~~

### Step 2: Next steps

The `stack new my-project` command in step one should have created the following
files and directories, among others. Click :material-plus-circle: to learn more
about each file:

~~~shell
.
├── app
│   └── Main.hs # (1)!
├── src
│   └── Lib.hs # (2)!
├── test
│   └── Spec.hs # (3)!
├── my-project.cabal # (4)!
├── package.yaml # (5)!
└── stack.yaml # (6)!
~~~

1.  The Haskell source code for the executable (application).

    As your project develops you can add further source code files to the `app`
    directory.

2.  The executable uses a library. The Haskell source code for the library.

    As your project develops you can add further source code files to the `src`
    directory.

3.  The package has a test suite executable. The Haskell source code for the
    test suite.

    As your project develops you can add further source code files to the `test`
    directory.

4.  A file describing the package in the Cabal format, including other packages
    on which depends. Stack generates it from the contents of the `package.yaml`
    file.

    If the `package.yaml` file is deleted, Stack will use the Cabal file.

5.  A file describing the package in the Hpack format. Stack generates the
    `my-project.cabal` file from its contents.

    If you want, you can delete the file and update the Cabal file directly.

    As your project develops, you may need to depend on a library provided by
    another Haskell package. If you do, add the name of that new package to
    the `dependencies:` section.

6.  Stack's project-level configuration. This specifies a snapshot that, in
    turn, specifies a version of GHC and a set of package versions chosen to
    work well together. It also identifies the local packages in the project.

    If you add a new package as a dependency in the package description, and
    Stack reports that the Stack configuration has no specified version for it,
    then follow Stack's likely recommended action to add a specific version to
    the `extra-deps:` section.

That was a really fast introduction on how to start to code in Haskell using
Stack. If you want to go further, we recommend you read Stack's guide to
[getting started](tutorial/index.md).

## Complete guide to Stack

A complete guide to Stack is available, covering the most common ways to
[use Stack](tutorial/index.md), its [commands](commands/index.md), its
[configuration](configure/index.md), specific [topics](topics/index.md), and
[frequently asked questions](faq.md). Terms used in Stack's documentation are
also explained in the [glossary](glossary.md).

## Why Stack?

Stack has a strong focus on plans for building that are reproducible; projects
that have more than one package; and a consistent, easy-to-learn set of
Stack commands. It also aims to provide the ability to customise and power that
experienced developers need.

Stack does not stand alone. It is built on the great work provided by:

<div class="grid cards" markdown>

-   :fontawesome-solid-gears:{ .lg .middle } __Glasgow Haskell Compiler__

    The premier Haskell compiler. Stack will manage your GHC
    installations and automatically select the appropriate version of GHC for
    your project.

    ---

    [:octicons-arrow-right-24: Learn more](https://www.haskell.org/ghc/)

-   :fontawesome-solid-trowel-bricks:{ .lg .middle } __Cabal build system__

    A specification for defining Haskell packages and a library for performing
    builds.[^2]

    [^2]:
        Cabal is also the name of a tool used for building Haskell code,
        provided by the `cabal-install` package. This guide distinguishes
        between them by Cabal (the library) and Cabal (the tool).

    ---

    [:octicons-arrow-right-24: Learn more](https://hackage.haskell.org/package/Cabal)

-   :octicons-database-24:{ .lg .middle } __Hackage__

    A repository of Haskell packages providing thousands of open source
    libraries and applications to help you get your work done.

    ---

    [:octicons-arrow-right-24: Learn more](https://hackage.haskell.org/)

-   :fontawesome-solid-cubes-stacked:{ .lg .middle } __Stackage__

    Sets of packages from Hackage that are chosen to work well together and
    with a specific version of GHC.

    ---

    [:octicons-arrow-right-24: Learn more](https://www.stackage.org/)

</div>

Stack is provided by a team of volunteers and companies under the auspices of
the [Commercial Haskell](http://commercialhaskell.com/) group. The project was
originally spearheaded by [FP Complete](https://www.fpcomplete.com/) to answer
the needs of commercial Haskell users. It has since become a thriving open
source project meeting the needs of Haskell users of all types.

## Questions?

For answers to frequently asked questions about Stack, please see the
[FAQ](faq.md).

For general questions please post to the
[Haskell Community](https://discourse.haskell.org/about) forum.

## Get involved!

Follow the advice under [get involved](community/index.md) for feedback and
discussion about Stack, or if you want to know how to contribute to its
maintenance or development.
