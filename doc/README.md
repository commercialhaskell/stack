<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

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

For many Un*x operating systems, all you need to do is run:

    curl -sSL https://get.haskellstack.org/ | sh

or:

    wget -qO- https://get.haskellstack.org/ | sh

On Windows, you can download and install the
[Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer).

For detailed instructions and downloads, including many additional
operating systems, check out the
[install and upgrade page](install_and_upgrade.md).

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

   - Add the package `text` to the file `my-project.cabal`
     in the section `build-depends: ...`.
   - Run `stack build` another time.

3. If you get an error that tells you your package isn't in the LTS.
   Just try to add a new version in the `stack.yaml` file in the `extra-deps` section.

That was a really fast introduction on how to start to code in Haskell using `stack`.
If you want to go further, we highly recommend you to read the [`stack` guide](GUIDE.md).

#### How to contribute

This assumes that you have already installed a version of stack, and have `git`
installed.

1. Clone `stack` from git with
   `git clone https://github.com/commercialhaskell/stack.git`.
2. Enter into the stack folder with `cd stack`.
3. Build `stack` using a pre-existing `stack` install with
   `stack setup && stack build`.
4. Once `stack` finishes building, check the stack version with
   `stack exec stack -- --version`. Make sure the version is the latest.
5. Look for issues tagged with
   [newcomer friendly](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3a%22newcomer+friendly%22)
   and
   [awaiting pull request](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pull+request%22)
   labels.

Build from source as a one-liner:

```bash
git clone https://github.com/commercialhaskell/stack.git && \
cd stack && \
stack setup && \
stack build
```

If you need to check your changes quickly run:

```bash
stack ghci
λ: :main --stack-root /path/to/root/ --stack-yaml /path/to/stack.yaml COMMAND
```

This allows you to set a special stack root (instead of `~/.stack/`) and to
target your commands at a particular `stack.yaml` instead of the one found in
the current directory.

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
