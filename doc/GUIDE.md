  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# User guide (introductory)

Stack is a modern, cross-platform build tool for Haskell code.

This introductory guide takes a new Stack user through the typical workflows.
This guide will not teach Haskell or involve much code, and it requires no prior
experience with the Haskell packaging system or other build tools. Terms used in
the guide are defined in the [glossary](glossary.md).

Some of Stack's features will not be needed regularly or by all users. See the
[advanced user's guide](GUIDE_advanced.md) for information about those features.

## Stack's functions

Stack handles the management of your toolchain (including GHC — the Glasgow
Haskell Compiler — and, for Windows users, MSYS2), building and registering
libraries, building build tool dependencies, and more. While it can use existing
tools on your system, Stack has the capacity to be your one-stop shop for all
Haskell tooling you need. This guide will follow that Stack-centric approach.

### What makes Stack special?

The primary Stack design point is __reproducible builds__. If you run
`stack build` today, you should get the same result running `stack build`
tomorrow. There are some cases that can break that rule (changes in your
operating system configuration, for example), but, overall, Stack follows this
design philosophy closely. To make this a simple process, Stack uses curated
package sets called __snapshots__.

Stack has also been designed from the ground up to be user friendly, with an
intuitive, discoverable command line interface. For many users, simply
downloading Stack and reading `stack --help` will be enough to get up and
running. This guide provides a more gradual tour for users who prefer that
learning style.

To build your project, Stack uses a project-level configuration file, named
`stack.yaml`, in the root directory of your project as a sort of blueprint. That
file contains a reference, called a __resolver__, to the snapshot which your
package will be built against.

Finally, Stack is __isolated__: it will not make changes outside of specific
Stack directories. Stack-built files generally go in either the Stack root
directory or `./.stack-work` directories local to each project. The
[Stack root](stack_root.md) directory holds packages belonging to snapshots and
any Stack-installed versions of GHC. Stack will not tamper with any system
version of GHC or interfere with packages installed by other build tools, such
as Cabal (the tool).

## Downloading and Installation

The [documentation dedicated to downloading Stack](install_and_upgrade.md) has
the most up-to-date information for a variety of operating systems. Instead of
repeating that content here, please go check out that page and come back here
when you can successfully run `stack --version`.

We also assume that the directory reported by `stack path --local-bin` has been
added to the PATH.

## Hello World Example

With Stack installed, let's create a new project from a template and walk
through the most common Stack commands.

In this guide, an initial `$` represents the command line prompt. The prompt may
differ in the terminal on your operating system. Unless stated otherwise, the
working directory is the project's root directory.

### The `stack new` command

We'll start off with the `stack new` command to create a new *project*, that
will contain a Haskell *package* of the same name. So let's pick a valid
package name first:

> A package is identified by a globally-unique package name, which consists
> of one or more alphanumeric words separated by hyphens. To avoid ambiguity,
> each of these words should contain at least one letter.

(From the
[Cabal users guide](https://www.haskell.org/cabal/users-guide/developing-packages.html#developing-packages))

We'll call our project `helloworld`, and we'll use the `new-template` project
template. This template is used by default, but in our example we will refer to
it expressly. Other templates are available. For further information about
templates, see the `stack templates` command
[documentation](templates_command.md).

From the root directory for all our Haskell projects, we command:

~~~text
stack new helloworld new-template
~~~

For this first Stack command, there's quite a bit of initial setup it needs to
do (such as downloading the list of packages available upstream), so you'll see
a lot of output. Over the course of this guide a lot of the content will begin
to make more sense.

After creating the project directory, and obtaining and populating the project
template, Stack will initialise its own project-level configuration. For further
information about setting paramaters to populate templates, see the YAML
configuration [documentation](yaml_configuration.md#templates). For further
information about initialisation, see the `stack init` command
[documentation](#the-stack-init-command). The `stack new` and `stack init`
commands have options and flags in common.

!!! info

    Pass the `--bare` flag to cause Stack to create the project in the current
    working directory rather than in a new project directory.

!!! info

    Parameters to populate project templates can be set at the command line with
    the `--param <key>:<value>` (or `-p`) option.

We now have a project in the `helloworld` directory! We will change to that
directory, with command:

~~~text
cd helloworld
~~~

### The `stack build` command

Next, we'll run the most important Stack command, `stack build`:

~~~text
stack build
# installing ... building ...
~~~

Stack needs a version of GHC in order to build your project. Stack will discover
that you are missing it and will install it for you.

You'll get intermediate download percentage statistics while the download is
occurring. This command may take some time, depending on download speeds.

!!! note

    GHC will be installed to your Stack programs directory, so calling `ghc` on
    the command line won't work. See the `stack exec`, `stack ghc`, and
    `stack runghc` commands below for more information.

Once a version of GHC is installed, Stack will then build your project.

### The `stack exec` command

Looking closely at the output of the previous command, you can see that it built
both a library called `helloworld` and an executable called `helloworld-exe` (on
Windows, `helloworld-exe.exe`). We'll explain more in the next section, but, for
now, just notice that the executables are installed in a location in our
project's `.stack-work` directory.

Now, Let's use the `stack exec` command to run our executable (which just
outputs "someFunc"):

~~~text
stack exec helloworld-exe
someFunc
~~~

`stack exec` works by providing the same reproducible environment that was used
to build your project to the command that you are running. Thus, it knew where
to find `helloworld-exe` even though it is hidden in the `.stack-work`
directory. Command `stack path --bin-path` to see the PATH in the Stack
environment.

!!! info

    On Windows, the Stack environment includes the `\mingw64\bin`, `\usr\bin`
    and `\usr\local\bin` directories of the Stack-supplied MSYS2. If your
    executable depends on files (for example, dynamic-link libraries) in those
    directories and you want ro run it outside of the Stack environment, you
    will need to ensure copies of those files are on the PATH.

### The `stack test` command

Finally, like all good software, `helloworld` actually has a test suite.

Let's run it with the `stack test` command:

~~~text
stack test
# build output ...
~~~

Reading the output, you'll see that Stack first builds the test suite and then
automatically runs it for us. For both the `build` and `test` command, already
built components are not built again. You can see this by using the
`stack build` and `stack test` commands a second time:

~~~text
stack build
stack test
# build output ...
~~~

## Inner Workings of Stack

In this subsection, we'll dissect the `helloworld` example in more detail.

### Files in helloworld

Before studying Stack more, let's understand our project a bit better. The files
in the directory include:

~~~text
app/Main.hs
src/Lib.hs
test/Spec.hs
ChangeLog.md
README.md
LICENSE
Setup.hs
helloworld.cabal
package.yaml
stack.yaml
.gitignore
~~~

The `app/Main.hs`, `src/Lib.hs`, and `test/Spec.hs` files are all Haskell
source files that compose the actual functionality of our project (we won't
dwell on them here).

The `ChangeLog.md`, `README.md`, `LICENSE` and `.gitignore` files have no effect
on the build.

The `helloworld.cabal` file is updated automatically as part of the
`stack build` process and should not be modified.

The files of interest here are `Setup.hs`, `stack.yaml`, and `package.yaml`.

The `Setup.hs` file is a component of the Cabal build system which Stack uses.
It's technically not needed by Stack, but it is still considered good practice
in the Haskell world to include it. The file we're using is straight
boilerplate:

~~~haskell
import Distribution.Simple
main = defaultMain
~~~

Next, let's look at our `stack.yaml` file, which gives our project-level
settings. Ignoring comments beginning `#`, the contents will look something like
this:

~~~yaml
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/20/19.yaml
packages:
- .
~~~

The value of the `resolver` key tells Stack *how* to build your package: which
GHC version to use, versions of package dependencies, and so on. Our value here
says to use [LTS Haskell 20.19](https://www.stackage.org/lts-20.19), which
implies GHC 9.2.7 (which is why `stack build` installs that version of GHC if it
is not already available to Stack). There are a number of values you can use for
`resolver`, which we'll cover later.

The value of the `packages` key tells Stack which local packages to build. In
our simple example, we have only a single package in our project, located in the
same directory, so '`.`' suffices. However, Stack has powerful support for
multi-package projects, which we'll elaborate on as this guide progresses.

Another file important to the build is `package.yaml`.

The `package.yaml` file describes the package in the
[Hpack](https://github.com/sol/hpack) format. Stack has in-built Hpack
functionality and this is its preferred package format. The default behaviour is
to generate the Cabal file (here named `helloworld.cabal`) from this
`package.yaml` file, and accordingly you should **not** modify the Cabal file.

It is also important to remember that Stack is built on top of the Cabal build
system. Therefore, an understanding of the moving parts in Cabal are necessary.
In Cabal, we have individual *packages*, each of which contains a single Cabal
file, named `<package_name>.cabal`. The Cabal file can define one or more
*components*: a library, executables, test suites, and benchmarks. It also
specifies additional information such as library dependencies, default
language pragmas, and so on.

In this guide, we'll discuss the bare minimum necessary to understand how to
modify a `package.yaml` file. You can see a full list of the available options
at the [Hpack documentation](https://github.com/sol/hpack#quick-reference). The
Cabal User Guide the definitive reference for the
[Cabal file format](https://cabal.readthedocs.io/en/stable/cabal-package.html).

### The location of GHC

As we saw above, the `build` command installed GHC for us. You can use the
`stack path` command for quite a bit of path information (which we'll play with
more later). We'll look at where GHC is installed:

=== "Unix-like"

    Command:

    ~~~text
    stack exec -- which ghc
    /home/<user_name>/.stack/programs/x86_64-linux/ghc-9.0.2/bin/ghc
    ~~~

=== "Windows (with PowerShell)"

    Command:

    ~~~text
    stack exec -- where.exe ghc
    C:\Users\<user_name>\AppData\Local\Programs\stack\x86_64-windows\ghc-9.0.2\bin\ghc.exe
    ~~~

As you can see from that path (and as emphasized earlier), the installation is
placed to not interfere with any other GHC installation, whether system-wide or
even different GHC versions installed by Stack.

## Cleaning your project

You can clean up build artifacts for your project using the `stack clean` and
`stack purge` commands.

### The `stack clean` command

`stack clean` deletes the local working directories containing compiler output.
By default, that means the contents of directories in `.stack-work/dist`, for
all the `.stack-work` directories within a project.

Use `stack clean <specific-package>` to delete the output for the package
_specific-package_ only.

### The `stack purge` command

`stack purge` deletes the local stack working directories, including extra-deps,
git dependencies and the compiler output (including logs). It does not delete
any snapshot packages, compilers or programs installed using `stack install`.
This essentially reverts the project to a completely fresh state, as if it had
never been built. `stack purge` is just a shortcut for `stack clean --full`

### The `stack build` command

The `build` command is the heart and soul of Stack. It is the engine that powers
building your code, testing it, getting dependencies, and more. Quite a bit of
the remainder of this guide will cover more advanced `build` functions and
features, such as building test and Haddocks at the same time, or constantly
rebuilding blocking on file changes.

!!! note

    Using the `build` command twice with the same options and arguments should
    generally do nothing (besides things like rerunning test suites), and
    should, in general, produce a reproducible result between different runs.

## Adding dependencies

Let's say we decide to modify our `helloworld` source a bit to use a new
library, perhaps the ubiquitous `text` package. In `src/Lib.hs`, we can, for
example add:

~~~haskell
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"
~~~

When we try to build this, things don't go as expected:

~~~text
stack build
# build failure output (abridged for clarity) ...
src\Lib.hs:6:1: error:
    Could not load module ‘Data.Text.IO’
    It is a member of the hidden package ‘text-1.2.5.0’.
    Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
6 | import qualified Data.Text.IO as T
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
~~~

This means that the package containing the module in question is not available.
To tell Stack to use [text](https://hackage.haskell.org/package/text), you need
to add it to your `package.yaml` file — specifically in your `dependencies`
section, like this:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text # added here
~~~

Now if we rerun `stack build`, we should get a successful result. Command:

~~~text
stack build
# build output ...
~~~

This output means that the `text` package was downloaded, configured, built, and
locally installed. Once that was done, we moved on to building our local package
(`helloworld`). At no point did we need to ask Stack to build dependencies — it
does so automatically.

### Listing Dependencies

Let's have Stack add a few more dependencies to our project. First, we'll
include two new packages in the `dependencies` section for our library in our
`package.yaml`:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text
- filepath
- containers
~~~

After adding these two dependencies, we can again run `stack build` to have them
installed. Command:

~~~text
stack build
# build output ...
~~~

Finally, to find out which versions of these libraries Stack installed, we can
ask Stack to `ls dependencies`. Command:

~~~text
stack ls dependencies
# dependency output ...
~~~

### extra-deps

Let's try a more off-the-beaten-track package: the joke
[acme-missiles](http://www.stackage.org/package/acme-missiles) package. Our
source code is simple:

~~~haskell
module Lib
    ( someFunc
    ) where

import Acme.Missiles

someFunc :: IO ()
someFunc = launchMissiles
~~~

Again, we add this new dependency to the `package.yaml` file like this:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text
- filepath
- containers
- acme-missiles # added
~~~

However, rerunning `stack build` shows us the following error message. Command:

~~~text
stack build
# build failure output ...
~~~

It says that it was unable to construct the build plan.

This brings us to the next major topic in using Stack.

## Curated package sets

Remember above when `stack new` selected some
[LTS resolver](https://github.com/commercialhaskell/lts-haskell#readme) for us?
That defined our build plan and available packages. When we tried using the
`text` package, it just worked, because it was part of the LTS *package set*.

We've specified the `acme-missiles` package in the `package.yaml` file (see
above), but `acme-missiles` is not part of that LTS package set, so building
failed.

To add `acme-missiles` to the available packages, we'll use the `extra-deps` key
in the `stack.yaml` file. That key defines extra packages, not present in the
resolver, that will be needed as dependencies. You can add this like so:

~~~yaml
extra-deps:
- acme-missiles-0.3 # not in the LTS resolver
~~~

Now `stack build` will succeed.

With that out of the way, let's dig a little bit more into these package sets,
also known as *snapshots*. We mentioned the LTS resolvers, and you can get quite
a bit of information about it at
[https://www.stackage.org/lts](https://www.stackage.org/lts), including:

* The appropriate resolver value (`resolver: lts-20.19`, as is currently the
  latest LTS)
* The GHC version used
* A full list of all packages available in this snapshot
* The ability to perform a Hoogle search on the packages in this snapshot
* A [list of all modules](https://www.stackage.org/lts/docs) in a snapshot,
  which can be useful when trying to determine which package to add to your
  `package.yaml` file.

You can also see a
[list of all available snapshots](https://www.stackage.org/snapshots). You'll
notice two flavors: LTS (for "Long Term Support") and Nightly. You can read more
about them on the
[LTS Haskell GitHub page](https://github.com/commercialhaskell/lts-haskell#readme).
If you're not sure which to use, start with LTS Haskell (which Stack will lean
towards by default as well).

## Resolvers and changing your compiler version

Let's explore package sets a bit further. Instead of `lts-20.19`, let's change
our `stack.yaml` file to use the
[latest nightly](https://www.stackage.org/nightly). Right now, this is currently
2023-05-05 - please see the resolver from the link above to get the latest.

Then, commanding `stack build` again will produce:

~~~text
stack build
# Downloaded nightly-2023-05-05 build plan.
# build output ...
~~~

We can also change resolvers on the command line, which can be useful in a
Continuous Integration (CI) setting, like on Travis. For example, command:

~~~text
stack --resolver lts-18.28 build
# Downloaded lts-18.28 build plan.
# build output ...
~~~

When passed on the command line, you also get some additional "short-cut"
versions of resolvers: `--resolver nightly` will use the newest Nightly resolver
available, `--resolver lts` will use the newest LTS, and `--resolver lts-20`
will use the newest LTS in the 20.x series. The reason these are only available
on the command line and not in your `stack.yaml` file is that using them:

1. Will slow down your build (since Stack then needs to download information on
   the latest available LTS each time it builds)
2. Produces unreliable results (since a build run today may proceed differently
   tomorrow because of changes outside of your control)

### Changing GHC versions

Finally, let's try using an older LTS snapshot. We'll use the newest 19.x
snapshot with the command:

~~~text
stack --resolver lts-19 build
# build output ...
~~~

This succeeds, automatically installing the necessary GHC along the way. So, we
see that different LTS versions use different GHC versions and Stack can handle
that.

### Other resolver values

We've mentioned `nightly-YYYY-MM-DD` and `lts-X.Y` values for the resolver.
There are actually other options available, and the list will grow over time.
At the time of writing:

* `ghc-X.Y.Z`, for requiring a specific GHC version but no additional packages
* Experimental custom snapshot support

The most up-to-date information can always be found in the
[stack.yaml documentation](yaml_configuration.md#resolver).

## Existing projects

Alright, enough playing around with simple projects. Let's take an open source
package and try to build it. We'll be ambitious and use
[yackage](https://hackage.haskell.org/package/yackage), a local package server
using [Yesod](http://www.yesodweb.com/). To get the code, we'll use the
`stack unpack` command from the root directory for all our Haskell projects:

~~~text
stack unpack yackage
Unpacked yackage-0.8.1 to <root_directory>/yackage-0.8.1/
~~~

You can also unpack to the directory of your liking instead of the current one
by issuing the command:

~~~text
stack unpack yackage --to <desired_directory>
~~~

This will create a `yackage-0.8.1` directory inside `<desired_directory>`.

We will change to that directory, with the command:

~~~text
cd yackage-0.8.1
~~~

### The `stack init` command

This new directory does not have a `stack.yaml` file, so we need to make one
first. We could do it by hand, but let's be lazy instead with the `stack init`
command:

~~~text
stack init
# init output ...
~~~

`stack init` does quite a few things for you behind the scenes:

* Finds all of the Cabal files in your current directory and subdirectories
  (unless you use `--ignore-subdirs`) and determines the packages and versions
  they require
* Finds the best combination of snapshot and package flags that allows
  everything to compile with minimum external dependencies
* It tries to look for the best matching snapshot from latest LTS, latest
  nightly, other LTS versions in that order

Assuming it finds a match, it will write your `stack.yaml` file, and everything
will work.

!!! note

    The `yackage` package does not currently support Hpack, but you can also use
    `hpack-convert` should you need to generate a `package.yaml` file.

#### Excluded Packages

Sometimes multiple packages in your project may have conflicting requirements.
In that case `stack init` will fail, so what do you do?

You could manually create `stack.yaml` by omitting some packages to resolve the
conflict. Alternatively you can ask `stack init` to do that for you by
specifying `--omit-packages` flag on the command line. Let's see how that
works.

To simulate a conflict we will use `acme-missiles-0.3` in `yackage` and we will
also copy `yackage.cabal` to another directory and change the name of the file
and package to `yackage-test`. In this new package we will use
`acme-missiles-0.2` instead. Let's see what happens when we command `stack init`
again:

~~~text
stack init --force --omit-packages
# init failure output ...
~~~

Looking at `stack.yaml`, you will see that the excluded packages have been
commented out under the `packages` field. In case wrong packages are excluded
you can uncomment the right one and comment the other one.

Packages may get excluded due to conflicting requirements among user packages or
due to conflicting requirements between a user package and the resolver
compiler. If all of the packages have a conflict with the compiler then all of
them may get commented out.

When packages are commented out you will see a warning every time you run a
command which needs the configuration file. The warning can be disabled by
editing the configuration file and removing it.

#### Using a specific resolver

Sometimes you may want to use a specific resolver for your project instead of
`stack init` picking one for you. You can do that by using
`stack init --resolver <resolver>`.

You can also init with a compiler resolver if you do not want to use a snapshot.
That will result in all of your project's dependencies being put under the
`extra-deps` section.

#### Installing the compiler

Stack will automatically install the compiler when you run `stack build` but you
can manually specify the compiler by running `stack setup <GHC-VERSION>`.

#### Miscellaneous and diagnostics

_Add selected packages_: If you want to use only selected packages from your
project directory you can do so by explicitly specifying the package directories
on the command line.

_Duplicate package names_: If multiple packages under the directory tree have
same name, `stack init` will report those and automatically ignore one of them.

_Ignore subdirectories_: By default `stack init` searches all the subdirectories
for Cabal files. If you do not want that then you can use `--ignore-subdirs`
command line switch.

_Cabal warnings_: `stack init` will show warnings if there were issues in
reading a Cabal file. You may want to pay attention to the warnings as sometimes
they may result in incomprehensible errors later on during dependency solving.

_Package naming_: If the `Name` field defined in a Cabal file does not match
with the Cabal file name then `stack init` will refuse to continue.

_User warnings_: When packages are excluded or external dependencies added Stack
will show warnings every time the configuration file is loaded. You can suppress
the warnings by editing the configuration file and removing the warnings from
it. You may see something like this:

~~~text
stack build
Warning: Some packages were found to be incompatible with the resolver and have been left commented out in the packages section.
Warning: Specified resolver could not satisfy all dependencies. Some external packages have been added as dependencies.
You can suppress this message by removing it from stack.yaml
~~~

## Different databases

Time to take a short break from hands-on examples and discuss a little
architecture. Stack has the concept of multiple *databases*.

A database consists of a GHC package database (which contains the compiled
version of a library), executables, and a few other things as well. To give you
an idea, the contents of the parent directory of the `stack path --local-pkg-db`
directory are the directories:

~~~text
bin
doc
lib
pkgdb
~~~

Databases in Stack are *layered*. For example, the database listing we just gave
is called a *local* database (also known as a *mutable* database). That is
layered on top of a *snapshot* database (also known as a *write-only* database).
The snapshot database contains the libraries and executables that are considered
to be *immutable*. Finally, GHC itself ships with a number of libraries and
executables, also considered to be immutable, which forms the *global* database.

To get a quick idea of this, we can look at the output of the
`stack exec -- ghc-pkg list` command in our `helloworld` project:

~~~text
<stack path --global-pkg-db directory>
    Cabal-3.6.3.0
    Win32-2.12.0.1
    array-0.5.4.0
    base-4.16.2.0
    binary-0.8.9.0
    bytestring-0.11.3.1
    containers-0.6.5.1
    deepseq-1.4.6.1
    directory-1.3.6.2
    exceptions-0.10.4
    filepath-1.4.2.2
    (ghc-9.2.3)
    ghc-bignum-1.2
    ghc-boot-9.2.3
    ghc-boot-th-9.2.3
    ghc-compact-0.1.0.0
    ghc-heap-9.2.3
    ghc-prim-0.8.0
    ghci-9.2.3
    haskeline-0.8.2
    hpc-0.6.1.0
    integer-gmp-1.1
    libiserv-9.2.3
    mtl-2.2.2
    parsec-3.1.15.0
    pretty-1.1.3.6
    process-1.6.13.2
    rts-1.0.2
    stm-2.5.0.2
    template-haskell-2.18.0.0
    text-1.2.5.0
    time-1.11.1.1
    transformers-0.5.6.2
    xhtml-3000.2.2.1

<stack path --snapshot-pkg-db directory>
    acme-missiles-0.3

<stack path --local-pkg-db directory>
    helloworld-0.1.0.0
~~~

where `<stack path --global-pkg-db directory>` refers to the directory output by
the command `stack path --global-pkg-db`, and so on.

Notice that `acme-missiles` ends up in the *snapshot* database. Any package
which comes from Hackage, an archive, or a repository is considered to be an
*immutable* package.

Anything which is considered *mutable*, or depends on something mutable, ends up
in the *local* database. This includes your own code and any other packages
located on a local file path.

The reason we have this structure is that:

* it lets multiple projects reuse the same binary builds of immutable packages,
* but doesn't allow different projects to "contaminate" each other by putting
  non-standard content into the shared snapshot database.

As you probably guessed, there can be multiple snapshot databases available. See
the contents of the `snapshots` directory in the [Stack root](stack_root.md).

* On Unix-like operating systems, each snapshot is in the last of a sequence of
  three subdirectories named after the platform, a 256-bit hash of the source
  map (how the package should be built -- including the compiler, options, and
  immutable dependencies), and the GHC version.

* On Windows, each snapshot is in a subdirectory that is a shorter hash (eight
  characters) of the sequence of three directories used on Unix-like operating
  systems. This is done to avoid problems created by default limits on file
  path lengths on Windows systems.

These snapshot databases don't get layered on top of each other; they are each
used separately.

In reality, you'll rarely — if ever — interact directly with these databases,
but it's good to have a basic understanding of how they work so you can
understand why rebuilding may occur at different points.

## The build synonyms

Let's look at a subset of the `stack --help` output:

~~~text
build    Build the package(s) in this directory/configuration
install  Shortcut for 'build --copy-bins'
test     Shortcut for 'build --test'
bench    Shortcut for 'build --bench'
haddock  Shortcut for 'build --haddock'
~~~

Four of these commands are just synonyms for the `build` command. They are
provided for convenience for common cases (e.g., `stack test` instead of
`stack build --test`) and so that commonly expected commands just work.

What's so special about these commands being synonyms? It allows us to make
much more composable command lines. For example, we can have a command that
builds executables, generates Haddock documentation (Haskell API-level docs),
and builds and runs your test suites, with:

~~~text
stack build --haddock --test
~~~

You can even get more inventive as you learn about other flags. For example,
take the following command:

~~~text
stack build --pedantic --haddock --test --exec "echo Yay, it succeeded" --file-watch
~~~

This command will:

* turn on all warnings and errors (the `--pedantic` flag)
* build your library and executables
* generate Haddocks (the `--haddock` flag)
* build and run your test suite (the `--test` flag)
* run the command `echo Yay, it succeeded` when that completes (the `--exec`
  option)
* after building, watch for changes in the files used to build the project, and
  kick off a new build when done (the `--file-watch` flag)

### The `stack install` command and `copy-bins` option

It's worth calling out the behavior of the `install` command and `--copy-bins`
option, since this has confused a number of users (especially when compared to
behavior of other tools like Cabal (the tool)). The `install` command does
precisely one thing in addition to the build command: it copies any generated
executables to the local binary directory. You may recognize the default value
for that path:

On Unix-like operating systems, command:

~~~text
stack path --local-bin
/home/<user_name>/.local/bin
~~~

On Windows, command:

~~~text
stack path --local-bin
C:\Users\<user_name>\AppData\Roaming\local\bin
~~~

That's why the download page recommends adding that directory to your PATH. This
feature is convenient, because now you can simply run `executable-name` in your
shell instead of having to run `stack exec executable-name` from inside your
project directory.

Since it's such a point of confusion, let me list a number of things Stack does
*not* do specially for the `install` command:

* Stack will always build any necessary dependencies for your code. The install
  command is not necessary to trigger this behavior. If you just want to build a
  project, run `stack build`.
* Stack will *not* track which files it's copied to your local binary directory
  nor provide a way to automatically delete them. There are many great tools out
  there for managing installation of binaries, and Stack does not attempt to
  replace those.
* Stack will not necessarily be creating a relocatable executable. If your
  executables hard-codes paths, copying the executable will not change those
  hard-coded paths.

  * At the time of writing, there's no way to change those kinds of paths with
    Stack, but see
    [issue #848 about --prefix](https://github.com/commercialhaskell/stack/issues/848)
    for future plans.

That's really all there is to the `install` command: for the simplicity of what
it does, it occupies a much larger mental space than is warranted.

## Targets, locals, and extra-deps

We haven't discussed this too much yet, but, in addition to having a number of
synonyms *and* taking a number of options on the command line, the `build`
command *also* takes many arguments. These are parsed in different ways, and can
be used to achieve a high level of flexibility in telling Stack exactly what you
want to build.

We're not going to cover the full generality of these arguments here; instead,
there's documentation covering the full
[build command syntax](build_command.md). Here, we'll just point out a few
different types of arguments:

* You can specify a *package name*, e.g. `stack build vector`.
    * This will attempt to build the `vector` package, whether it's a local
      package, in your extra-deps, in your snapshot, or just available upstream.
      If it's just available upstream but not included in your locals,
      extra-deps, or snapshot, the newest version is automatically promoted to
      an extra-dep.
* You can also give a *package identifier*, which is a package name plus
  version, e.g. `stack build yesod-bin-1.4.14`.
    * This is almost identical to specifying a package name, except it will (1)
      choose the given version instead of latest, and (2) error out if the given
      version conflicts with the version of a local package.
* The most flexibility comes from specifying individual *components*, e.g.
  `stack build helloworld:test:helloworld-test` says "build the test suite
  component named helloworld-test from the helloworld package."
    * In addition to this long form, you can also shorten it by skipping what
      type of component it is, e.g. `stack build helloworld:helloworld-test`, or
      even skip the package name entirely, e.g. `stack build :helloworld-test`.
* Finally, you can specify individual *directories* to build to trigger building
  of any local packages included in those directories or subdirectories.

When you give no specific arguments on the command line (e.g., `stack build`),
it's the same as specifying the names of all of your local packages. If you
just want to build the package for the directory you're currently in, you can
use `stack build .`.

### Components, --test, and --bench

Here's one final important yet subtle point. Consider our `helloworld` package:
it has a library component, an executable `helloworld-exe`, and a test suite
`helloworld-test`. When you run `stack build helloworld`, how does it know which
ones to build? By default, it will build the library (if any) and all of the
executables but ignore the test suites and benchmarks.

This is where the `--test` and `--bench` flags come into play. If you use them,
those components will also be included. So `stack build --test helloworld` will
end up including the helloworld-test component as well.

You can bypass this implicit adding of components by being much more explicit,
and stating the components directly. For example, the following will not build
the `helloworld-exe` executable once all executables have been successfully
built:

~~~text
stack clean
stack build :helloworld-test
Building all executables for `helloworld' once. After a successful build of all of them, only specified executables will be rebuilt.
helloworld> configure (lib + exe + test)
Configuring helloworld-0.1.0.0...
helloworld> build (lib + exe + test)
Preprocessing library for helloworld-0.1.0.0..
Building library for helloworld-0.1.0.0..
[1 of 2] Compiling Lib
[2 of 2] Compiling Paths_helloworld
Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0..
Building executable 'helloworld-exe' for helloworld-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_helloworld
Linking .stack-work\dist\<hash>\build\helloworld-exe\helloworld-exe.exe ...
Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0..
Building test suite 'helloworld-test' for helloworld-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_helloworld
Linking .stack-work\dist\<hash>\build\helloworld-test\helloworld-test.exe ...
helloworld> copy/register
Installing library in ...\helloworld\.stack-work\install\...
Installing executable helloworld-exe in ...\helloworld\.stack-work\install\...\bin
Registering library for helloworld-0.1.0.0..
helloworld> test (suite: helloworld-test)

Test suite not yet implemented

helloworld> Test suite helloworld-test passed
Completed 2 action(s).
~~~

We first cleaned our project to clear old results so we know exactly what Stack
is trying to do. Note that it says it is building all executables for
`helloworld` once, and that after a successful build of all of them, only
specified executables will be rebuilt. If we change the source code of
`test/Spec.hs`, say to:

~~~haskell
main :: IO ()
main = putStrLn "Test suite still not yet implemented"
~~~

and command again:

~~~text
stack build :helloworld-test
helloworld-0.1.0.0: unregistering (local file changes: test\Spec.hs)
helloworld> build (lib + test)
Preprocessing library for helloworld-0.1.0.0..
Building library for helloworld-0.1.0.0..
Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0..
Building test suite 'helloworld-test' for helloworld-0.1.0.0..
[2 of 2] Compiling Main
Linking .stack-work\dist\<hash>\build\helloworld-test\helloworld-test.exe ...
helloworld> copy/register
Installing library in ...\helloworld\.stack-work\install\...
Installing executable helloworld-exe in ...\helloworld\.stack-work\install\...\bin
Registering library for helloworld-0.1.0.0..
helloworld> blocking for directory lock on ...\helloworld\.stack-work\dist\<hash>\build-lock
helloworld> test (suite: helloworld-test)

Test suite still not yet implemented

helloworld> Test suite helloworld-test passed
Completed 2 action(s).
~~~

Notice that this time it builds the `helloworld-test` test suite, and the
`helloworld` library (since it's used by the test suite), but it does not build
the `helloworld-exe` executable.

And now the final point: in both cases, the last line shows that our command
also *runs* the test suite it just built. This may surprise some people who
would expect tests to only be run when using `stack test`, but this design
decision is what allows the `stack build` command to be as composable as it is
(as described previously). The same rule applies to benchmarks. To spell it out
completely:

* The `--test` and `--bench` flags simply state which components of a package
  should be built, if no explicit set of components is given
* The default behavior for any test suite or benchmark component which has been
  built is to also run it

You can use the `--no-run-tests` and `--no-run-benchmarks` flags to disable
running of these components. You can also use `--no-rerun-tests` to prevent
running a test suite which has already passed and has not changed.

!!! note

    Stack doesn't build or run test suites and benchmarks for non-local
    packages. This is done so that a command like `stack test` doesn't need to
    run 200 test suites!

## Multi-package projects

Until now, everything we've done with Stack has used a single-package project.
However, Stack's power truly shines when you're working on multi-package
projects. All the functionality you'd expect to work just does: dependencies
between packages are detected and respected, dependencies of all packages are
just as one cohesive whole, and if anything fails to build, the build commands
exits appropriately.

Let's demonstrate this with the `wai-app-static` and `yackage` packages,
starting in the root directory for all our Haskell projects. Command:

~~~text
mkdir multi
cd multi
stack unpack wai-app-static yackage
Unpacked wai-app-static (from Hackage) to .../multi/wai-app-static-3.1.7.4/
Unpacked yackage (from Hackage) to .../multi/yackage-0.8.1/
stack init
Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- wai-app-static-3.1.7.4/
- yackage-0.8.1/

Cabal file warning in .../multi/yackage-0.8.1/yackage.cabal@47:40: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
Cabal file warning in .../multi/yackage-0.8.1/yackage.cabal@21:36: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
Selecting the best among 18 snapshots...

* Matches ...

Selected resolver: ...
Initialising configuration using resolver: ...
Total number of user packages considered: 2
Writing configuration to file: stack.yaml
stack build --haddock --test
# Goes off to build a whole bunch of packages
~~~

If you look at the `stack.yaml` file, you'll see exactly what you'd expect:

~~~yaml
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/17.yaml
packages:
- wai-app-static-3.1.7.4
- yackage-0.8.1
~~~

Notice that multiple directories are listed in the `packages` key.

In addition to local directories, you can also refer to packages available in a
Git repository or in a tarball over HTTP/HTTPS. This can be useful for using a
modified version of a dependency that hasn't yet been released upstream.

!!! note

    When adding upstream packages directly to your project it is important to
    distinguish _local packages_ from the upstream _dependency packages_.
    Otherwise you may have trouble running `stack ghci`. See
    [stack.yaml documentation](yaml_configuration.md#packages) for more details.

## Flags and GHC options

There are two common ways to alter how a package will install: with Cabal flags
and with GHC options.

### Cabal flag management

To change a Cabal flag setting, we can use the command line `--flag` option. The
`yackage` package has an `upload` flag that is enabled by default. We can
command:

~~~text
stack build --flag yackage:-upload
~~~

This means: when compiling the `yackage` package, turn off the `upload` flag
(thus the `-` in `-upload`). Unlike other tools, Stack is explicit about which
package's flag you want to change. It does this for two reasons:

1. There's no global meaning for Cabal flags, and therefore two packages can
   use the same flag name for completely different things.
2. By following this approach, we can avoid unnecessarily recompiling snapshot
   packages that happen to use a flag that we're using.

You can also change flag values on the command line for extra-dep and snapshot
packages. If you do this, that package will automatically be promoted to an
extra-dep, since the build plan is different than what the plan snapshot
definition would entail.

### GHC options

GHC options follow a similar logic as in managing Cabal flags, with a few
nuances to adjust for common use cases. Let's consider the command:

~~~text
stack build --ghc-options="-Wall -Werror"
~~~

This will set the `-Wall -Werror` options for all *local targets*. Note that
this will not affect extra-dep and snapshot packages at all. This design
provides us with reproducible and fast builds.

(By the way: the above GHC options have a special convenience flag:
`--pedantic`.)

There's one extra nuance about command line GHC options: Since they only apply
to local targets, if you change your local targets, they will no longer apply
to other packages. Let's play around with an example from the `wai` repository,
which includes the `wai` and `warp` packages, the latter depending on the
former. If we command again:

~~~text
stack build --ghc-options=-O0 wai
~~~

It will build all of the dependencies of `wai`, and then build `wai` with all
optimizations disabled. Now let's add in `warp` as well. Command:

~~~text
stack build --ghc-options=-O0 wai warp
~~~

This builds the additional dependencies for `warp`, and then builds `warp` with
optimizations disabled. Importantly: it does not rebuild `wai`, since `wai`'s
configuration has not been altered. Now the surprising case. Command:

~~~text
stack build --ghc-options=-O0 warp
wai-3.0.3.0-5a49351d03cba6cbaf906972d788e65d: unregistering (flags changed from ["--ghc-options","-O0"] to [])
warp-3.1.3-a91c7c3108f63376877cb3cd5dbe8a7a: unregistering (missing dependencies: wai)
wai-3.0.3.0: configure
~~~

You may expect this to be a no-op: neither `wai` nor `warp` has changed.
However, Stack will instead recompile `wai` with optimizations enabled again,
and then rebuild `warp` (with optimizations disabled) against this newly built
`wai`. The reason: reproducible builds. If we'd never built `wai` or `warp`
before, trying to build `warp` would necessitate building all of its
dependencies, and it would do so with default GHC options (optimizations
enabled). This dependency would include `wai`. So when we command:

~~~text
stack build --ghc-options=-O0 warp
~~~

We want its behavior to be unaffected by any previous build steps we took.
While this specific corner case does catch people by surprise, the overall goal
of reproducible builds is - in the Stack maintainers' views - worth the
confusion.

Final point: if you have GHC options that you'll be regularly passing to your
packages, you can add them to your `stack.yaml` file. See the
[documentation section on ghc-options](yaml_configuration.md#ghc-options)
for more information.

!!! note

    That's it, the heavy content of this guide is done! Everything from here on
    out is simple explanations of commands. Congratulations!

## The `stack path` command

Generally, you don't need to worry about where Stack stores various files. But
some people like to know this stuff. That's when the `stack path` command is
useful. `stack path --help` explains the available options and, consequently,
the output of the command:

~~~text
--stack-root             Global Stack root directory
--global-config          Global Stack configuration file
--project-root           Project root (derived from stack.yaml file)
--config-location        Configuration location (where the stack.yaml file is)
--bin-path               PATH environment variable
--programs               Install location for GHC and other core tools (see
                         'stack ls tools' command)
--compiler-exe           Compiler binary (e.g. ghc)
--compiler-bin           Directory containing the compiler binary (e.g. ghc)
--compiler-tools-bin     Directory containing binaries specific to a
                         particular compiler (e.g. intero)
--local-bin              Directory where Stack installs executables (e.g.
                         ~/.local/bin (Unix-like OSs) or %APPDATA%\local\bin
                         (Windows))
--extra-include-dirs     Extra include directories
--extra-library-dirs     Extra library directories
--snapshot-pkg-db        Snapshot package database
--local-pkg-db           Local project package database
--global-pkg-db          Global package database
--ghc-package-path       GHC_PACKAGE_PATH environment variable
--snapshot-install-root  Snapshot installation root
--local-install-root     Local project installation root
--snapshot-doc-root      Snapshot documentation root
--local-doc-root         Local project documentation root
--local-hoogle-root      Local project documentation root
--dist-dir               Dist work directory, relative to package directory
--local-hpc-root         Where HPC reports and tix files are stored
~~~

In addition, `stack path` accepts the flags above on the command line to state
which keys you're interested in. This can be convenient for scripting. As a
simple example, let's find out the sandboxed versions of GHC that Stack
installed:

=== "Unix-like"

    Command:

    ~~~text
    ls $(stack path --programs)/*.installed
    /home/<user_name>/.stack/programs/x86_64-linux/ghc-9.0.2.installed
    ~~~

=== "Windows (with PowerShell)"

    Command:

    ~~~text
    dir "$(stack path --programs)/*.installed"

    Directory: C:\Users\mikep\AppData\Local\Programs\stack\x86_64-windows

    Mode                 LastWriteTime         Length Name
    ----                 -------------         ------ ----
    -a---          27/07/2022  5:40 PM              9 ghc-9.0.2.installed
    -a---          25/02/2022 11:39 PM              9 msys2-20210604.installed
    ~~~

While we're talking about paths, to wipe our Stack install completely, here's
what typically needs to be removed:

1. the Stack root folder (see `stack path --stack-root`, before you uninstall);
2. if different, the folder containing Stack's global YAML configuration file
   (see `stack path --global-config`, before you uninstall);
3. on Windows, the folder containing Stack's tools (see `stack path --programs`,
   before you uninstall), which is located outside of the Stack root folder; and
4. the `stack` executable file (see `which stack`, on Unix-like operating
   systems, or `where.exe stack`, on Windows).

You may also want to delete `.stack-work` folders in any Haskell projects that
you have built using Stack. The `stack uninstall` command provides information
about how to uninstall Stack.

## The `stack exec` command

We've already used `stack exec` multiple times in this guide. As you've likely
already guessed, it allows you to run executables, but with a slightly modified
environment. In particular: `stack exec` looks for executables on Stack's bin
paths, and sets a few additional environment variables (like adding those paths
to the PATH, and setting `GHC_PACKAGE_PATH`, which tells GHC which package
databases to use).

If you want to see exactly what the modified environment looks like, try
command:

~~~text
stack exec env
~~~

The only issue is how to distinguish flags to be passed to Stack versus those
for the underlying program. Thanks to the `optparse-applicative` library, Stack
follows the Unix convention of `--` to separate these. For example, command:

~~~text
stack exec --package stm -- echo I installed the stm package via --package stm
~~~

yields output like:

~~~text
Run from outside a project, using implicit global project config
Using latest snapshot resolver: lts-20.19
Writing global (non-project-specific) config file to: /home/michael/.stack/global/stack.yaml
Note: You can change the snapshot via the resolver field there.
I installed the stm package via --package stm
~~~

Flags worth mentioning:

* `--package foo` can be used to force a package to be installed before running
  the given command.
* `--no-ghc-package-path` can be used to stop the `GHC_PACKAGE_PATH` environment
  variable from being set. Some tools — notably Cabal (the tool) — do not behave
  well with that variable set.

You may also find it convenient to use `stack exec` to launch a subshell
(substitute `bash` with your preferred shell) where your compiled executable is
available at the front of your PATH. Command:

~~~text
stack exec bash
~~~

## The `stack ghci` or `stack repl` command

GHCi is the interactive GHC environment, a.k.a. the REPL. You *could* access it
with command:

~~~text
stack exec ghci
~~~

But that won't load up locally written modules for access. For that, use the
`stack ghci` or `stack repl` commands, which are equivalent. To then load
modules from your project in GHCi, use the `:module` command (`:m` for short)
followed by the module name.

!!! note

    If you have added packages to your project please make sure to mark them as
    extra deps for faster and reliable usage of `stack ghci`. Otherwise GHCi may
    have trouble due to conflicts of compilation flags or having to
    unnecessarily interpret too many modules. See Stack's project-level
    [configuration](yaml_configuration.md#extra-deps) to learn how to
    configure a package as an extra-dep.

For further information, see the [REPL environment](ghci.md) documentation.

## The `stack ghc` and `stack runghc` commands

You'll sometimes want to just compile (or run) a single Haskell source file,
instead of creating an entire Cabal package for it. You can use `stack exec ghc`
or `stack exec runghc` for that. As simple helpers, we also provide the
`stack ghc` and `stack runghc` commands, for these common cases.

## Finding project configs, and the implicit global project

Whenever you run something with Stack, it needs a project-level configuration
file. The algorithm Stack uses to find such a file is:

1. Check for a `--stack-yaml` option on the command line
2. Check for a `STACK_YAML` environment variable
3. Check the current directory and all ancestor directories for a `stack.yaml`
   file

The first two provide a convenient method for using an alternate configuration.
For example: `stack build --stack-yaml stack-ghc-9.2.3.yaml` can be used by your
CI system to check your code against GHC 9.2.3. Setting the `STACK_YAML`
environment variable can be convenient if you're going to be running commands
like `stack ghc` in other directories, but you want to use the configuration you
defined in a specific project.

If Stack does not find a project level configuration file in any of the three
specified locations, the *implicit global* logic kicks in. You've probably
noticed that phrase a few times in the output from commands above. Implicit
global is essentially a hack to allow Stack to be useful in a non-project
setting. When no implicit global configuration file exists, Stack creates one
for you with the latest LTS snapshot as the resolver. This allows you to do
things like:

* compile individual files easily with `stack ghc`
* build executables without starting a project, e.g. `stack install pandoc`

Keep in mind that there's nothing magical about this implicit global
configuration. It has no effect on projects at all. Every package you install
with it is put into isolated databases just like everywhere else. The only magic
is that it's the catch-all project whenever you're running Stack somewhere else.

## `stack.yaml` versus Cabal files

Now that we've covered a lot of Stack use cases, this quick summary of
`stack.yaml` versus Cabal files will hopefully make sense and be a good reminder
for future uses of Stack:

* A project can have multiple packages.
* Each project has a `stack.yaml`.
* Each package has a Cabal file, named `<package_name>.cabal`.
* The Cabal file specifies which packages are dependencies.
* The `stack.yaml` file specifies which packages are available to be used.
* The Cabal file specifies the components, modules, and build flags provided by
  a package
* `stack.yaml` can override the flag settings for individual packages
* `stack.yaml` specifies which packages to include

## Comparison to other tools

Stack is not the only tool available for building Haskell code. Stack came into
existence due to limitations at that time with some of the existing tools. If
you are happily building Haskell code with other tools, you may not need Stack.
If you're experiencing problems with other tools, give Stack a try instead.

If you're a new user who has no experience with other tools, we recommend Stack.
The defaults match modern best practices in Haskell development, and there are
fewer corner cases you need to be aware of. You *can* develop Haskell code with
other tools, but you probably want to spend your time writing code, not
convincing a tool to do what you want.

### Underlying package format

Before turning to differences, we clarify an important similarity: Stack, Cabal
(the tool), and presumably all other tools share the same underlying package
format of Cabal (the library). This is a Good Thing: we can share the same set
of upstream libraries, and collaboratively work on the same project with Stack,
Cabal (the tool), and NixOS. In that sense, we're sharing the same ecosystem.

### Curation vs dependency solving

* Stack uses 'curation' (snapshots and Stack's project-level configuration file
  (`stack.yaml`) define precisely the set of packages available for a project).
  The Stack team firmly believes that the majority of users want to simply
  ignore dependency resolution nightmares and get a valid build plan from day
  one. That's why we've made 'curation' the focus of Stack.

* Cabal (the tool) can use 'curation' too but its origins are in dependency
  solving.

### Emphasis on reproducibility

* Stack goes to great lengths to ensure that `stack build` today does the
  same thing tomorrow. With Stack, changing the build plan is always an explicit
  decision.

* Cabal (the tool) does not go to the same lengths: build plans can be affected
  by the presence of pre-installed packages, and running `cabal update` can
  cause a previously successful build to fail.

### Automatic building of dependencies

*   Stack's automatically builds dependencies. So for example, in Stack,
    `stack test` does the same job as:

    ~~~text
    cabal install --enable-tests --only-dependencies
    cabal configure --enable-tests
    cabal build
    cabal test
    ~~~

    (newer versions of Cabal (the tool) may make this command sequence shorter).

*   With Cabal (the tool), you need to use `cabal install` to trigger dependency
    building. This is somewhat necessary as building dependencies can, in some
    cases, break existing installed packages.

### Isolation

* Stack is isolated - provides 'sandboxed' behaviour - by default, via its
  databases. In other words: when you use Stack, there's
  __no need for sandboxes__, everything is (essentially) sandboxed by default.

* With Cabal (the tool), the default behavior is a non-isolated build where
  working on two projects can cause the user package database to become
  corrupted. The Cabal solution to this is sandboxes.

### Tools other than Stack and Cabal (the tool)

* [cabal-meta](https://hackage.haskell.org/package/cabal-meta) inspired a lot of
  the multi-package functionality of Stack. Still relevant for Cabal (the
  tool).
* [cabal-src](https://hackage.haskell.org/package/cabal-src). Deprecated in
  favor of Stack in 2016.
* [stackage-cli](https://hackage.haskell.org/package/stackage-cli).Deprecated
  in favor of Stack in 2015.
* [cabal-dev](https://hackage.haskell.org/package/cabal-dev). Deprecated in
  favor of Cabal (the tool) in 2013.
