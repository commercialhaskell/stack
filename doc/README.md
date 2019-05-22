<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The Haskell Tool Stack

Stack is a cross-platform program for developing Haskell
projects. It is aimed at Haskellers both new and experienced.

<img src="https://i.imgur.com/WW69oTj.gif" width="50%" align="right">

It features:

* Installing GHC automatically, in an isolated location.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.

#### How to install

For most Un*x operating systems, the easiest way to install is to run:

    curl -sSL https://get.haskellstack.org/ | sh

or:

    wget -qO- https://get.haskellstack.org/ | sh

On Windows, you can download and install the
[Windows 64-bit Installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

For other operating systems and direct downloads, check out the
[install and upgrade guide](install_and_upgrade.md).

Note that the [get.haskellstack.org](https://get.haskellstack.org/)
script will ask for root access using `sudo` in order to use your
platform's package manager to install dependencies and to install to
`/usr/local/bin`.  If you prefer more control, follow the manual
installation instructions in the
[install and upgrade guide](install_and_upgrade.md).

#### Quick Start Guide

First you need to [install it (see previous section)](#how-to-install).

##### Start your new project:

```bash
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
```

- The `stack new` command will create a new directory containing all
  the needed files to start a project correctly.
- The `stack setup` will download the compiler if necessary in an isolated
  location (default `~/.stack`) that won't interfere with any system-level
  installations. (For information on installation paths, please use the
  `stack path` command.).
- The `stack build` command will build the minimal project.
- `stack exec my-project-exe` will execute the command.
- If you just want to install an executable using stack, then all you have to do
  is `stack install <package-name>`.

If you want to launch a REPL:

```bash
stack ghci
```

Run `stack` for a complete list of commands.

##### Workflow

The `stack new` command should have created the following files:

```
.
├── LICENSE
├── Setup.hs
├── app
│   └── Main.hs
├── my-project.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

    3 directories, 7 files
```

So to manage your library:

1. Edit files in the `src/` directory.

   The `app` directory should preferably contain only files related to
   executables.

2. If you need to include another library (for example the package
   [`text`](https://hackage.haskell.org/package/text)):

   - Add the package `text` to the file `package.yaml`
     in the section `dependencies: ...`.
   - Run `stack build` another time.
   - `stack build` will update my-project.cabal for you.
     If desired you can update the .cabal file manually
     and stack will use .cabal instead of package.yaml.

3. If you get an error that tells you your package isn't in the LTS.
   Just try to add a new version in the `stack.yaml` file in the `extra-deps` section.

That was a really fast introduction on how to start to code in Haskell using `stack`.
If you want to go further, we highly recommend you to read the [`stack` guide](GUIDE.md).

#### How to contribute

[1]: https://docs.haskellstack.org/en/stable/CONTRIBUTING/
[2]: https://docs.haskellstack.org/en/stable/CONTRIBUTING/#contributing-overview
[3]: https://docs.haskellstack.org/en/stable/CONTRIBUTING/#contributing-to-documentation
[4]: https://docs.haskellstack.org/en/stable/CONTRIBUTING/#contributing-to-code
[5]: https://github.com/commercialhaskell/stack/blob/master/CONTRIBUTING.md
[6]: https://github.com/commercialhaskell/stack/blob/master/CONTRIBUTING.md#contributing-overview
[7]: https://github.com/commercialhaskell/stack/blob/master/CONTRIBUTING.md#contributing-to-documentation
[8]: https://github.com/commercialhaskell/stack/blob/master/CONTRIBUTING.md#contributing-to-code

To begin your contribution, we suggest that you clone this repository to your 
local machine and build the Haskell Tool Stack program - you can find the 
specific steps for this in the [contributing overview section][2] 
of the [contributors guide][1]. Also, please refer to the [contributing to documentation section][3]
if you want to make changes to documentation, and the [contributing to code section][4]
if you want to make changes to the source code.

#### Complete guide to stack

This repository also contains a complete [user guide to using
stack](GUIDE.md), covering all of the most common use cases.


#### Questions, Feedback, Discussion

* For frequently asked questions about detailed or specific use-cases, please
  see [the FAQ](faq.md).
* For general questions, comments, feedback and support, please write
  to [the stack mailing list](https://groups.google.com/d/forum/haskell-stack).
* For bugs, issues, or requests, please
  [open an issue](https://github.com/commercialhaskell/stack/issues/new).
* When using Stack Overflow, please use [the haskell-stack
  tag](http://stackoverflow.com/questions/tagged/haskell-stack).

#### Why Stack?

Stack is a build tool for Haskell designed to answer the needs of
Haskell users new and experienced alike. It has a strong focus on
reproducible build plans, multi-package projects, and a consistent,
easy-to-learn interface, while providing the customizability and
power experienced developers need.  As a build tool, Stack does not
stand alone. It is built on the great work provided by:

* The __Glasgow Haskell Compiler__ (GHC), the premier Haskell
  compiler. Stack will manage your GHC installations and automatically
  select the appropriate compiler version for your project.
* The __Cabal build system__, a specification for defining Haskell
  packages, together with a library for performing builds.
* The __Hackage package repository__, providing more than ten thousand
  open source libraries and applications to help you get your work
  done.
* The __Stackage package collection__, a curated set of packages from
  Hackage which are regularly tested for compatibility. Stack defaults
  to using Stackage package sets to avoid dependency problems.

Stack is provided by a team of volunteers and companies under the
auspices of the [Commercial Haskell](http://commercialhaskell.com/)
group. The project was spearheaded by
[FP Complete](https://www.fpcomplete.com/) to answer the needs of
commercial Haskell users, and has since become a thriving open source
project meeting the needs of Haskell users of all stripes.

If you'd like to get involved with Stack, check out the
[newcomer friendly](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3a%22newcomer+friendly%22)
label on the Github issue tracker.

#### How to uninstall
Removing ``~/.stack`` and ``/usr/local/bin/stack`` should be sufficient. You may want to delete ``.stack-work`` folders in any Haskell projects that you have built.
