<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 1. A Hello World example

With Stack installed, let's create a new project and walk through the most
common Stack commands.

In this guide, unless stated otherwise, the working directory is the project's
root directory.

## The `stack new` command

We'll start off with the [`stack new`](../commands/new_command.md) command to
create a new *project* from a project template. We'll use the `new-template`
project template. This template is used by default, but in our example we will
refer to it expressly.

That template will create a project with a package of the same name. So, we need
to pick a name for the project that is a valid package name. We'll call our
project `helloworld`.

??? question "How do project packages relate to projects?"

    A project can have one or more packages. Each project package has its own
    root directory. In the case of a single-package project, the project
    directory and the package directory can be the same directory.

??? question "What is a valid package name?"

    A valid package name consists of one or more alphanumeric words separated by
    hyphens. Each word must contain at least one letter. That is, the word must
    not be interpreted as a number.

    The names of packages are intended to be unique.

??? question "Are other project templates available?"

    Yes. For further information about project templates, command:
    ~~~text
    stack templates
    ~~~

From the root directory for all our Haskell projects, we command:

~~~text
stack new helloworld new-template
~~~

For this first Stack command, Stack will do some setting up. For example, it
will create the [Stack root](../topics/stack_root.md) directory.

Other than any setting up, Stack will:
* create the project directory;
* download the project template;
* attempt to populate the project template based on parameters; and
* create and initialise Stack's project-level configuration file.

Unless the parameters have been configured, Stack will note that parameters were
needed by the template but not provided. That can be ignored for now.

??? question "How can I configure project template paramaters?"

    For further information, see the
    [`templates`](../configure/yaml/non-project.md#templates) non-project
    specific configuration option.

    As noted in Stack's output, parameters to populate project templates can
    also be set at the command line by using the options of the `stack new`
    command.

??? question "Can I create a new project in the current working directory?"

    Yes. Pass the `--bare` flag to cause Stack to create the project in the
    current working directory rather than in a new project directory.

We now have a project in the `helloworld` directory! We will change to that
directory, with command:

~~~text
cd helloworld
~~~

## The `stack build` command

Next, we'll run the most important Stack command,
[`stack build`](../commands/build_command.md). We command:

~~~text
stack build
~~~

Stack needs a version of GHC and, on Windows, a version of MSYS2, in order to
build your project. Stack will discover that you are missing it and will install
it for you.

You'll get intermediate download percentage statistics while the download is
occurring. This command may take some time, depending on download speeds.

??? question "Can I use that version of GHC by commanding `ghc`?"

    No. GHC will be installed to the Stack programs directory, which is likely
    not on the PATH, so commanding `ghc` will not work. However, that version of
    GHC can be used in the Stack environment. For more information, see the
    [`stack exec`](../commands/exec_command.md) command,
    [`stack ghc`](../commands/ghc_command.md) command, and
    [`stack runghc`](../commands/ghc_command.md) command documentation.

Once a version of GHC and, on Windows, a version of MSYS2, is installed, Stack
will then build your project. The end of the output should look similar to this:

=== "Unix-like"

    ~~~text
    ...
    helloworld> configure (lib + exe)
    Configuring helloworld-0.1.0.0...
    helloworld> build (lib + exe) with ghc-9.6.6
    Preprocessing library for helloworld-0.1.0.0..
    Building library for helloworld-0.1.0.0..
    [1 of 2] Compiling Lib
    [2 of 2] Compiling Paths_helloworld
    Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0..
    Building executable 'helloworld-exe' for helloworld-0.1.0.0..
    [1 of 2] Compiling Main
    [2 of 2] Compiling Paths_helloworld
    [3 of 3] Linking .stack-work/dist/x86_64-linux-tinfo6/ghc-9.6.6/build/helloworld-exe/helloworld-exe
    helloworld> copy/register
    Installing library in .../helloworld/.stack-work/install/x86_64-linux-tinfo6/a2caceceda039eb4f791856f85a68f9582d4daf3d0527344693ff3d1fcd92ba4/9.6.6/lib/x86_64-linux-ghc-9.6.6/helloworld-0.1.0.0-KFyX8zLxDvzLZURq3JaCVX
    Installing executable helloworld-exe in .../helloworld/.stack-work/install/x86_64-linux-tinfo6/a2caceceda039eb4f791856f85a68f9582d4daf3d0527344693ff3d1fcd92ba4/9.6.6/bin
    Registering library for helloworld-0.1.0.0..
    ~~~

=== "Windows"

    ~~~text
    ...
    helloworld> configure (lib + exe)
    Configuring helloworld-0.1.0.0...
    helloworld> build (lib + exe) with ghc-9.6.6
    Preprocessing library for helloworld-0.1.0.0..
    Building library for helloworld-0.1.0.0..
    [1 of 2] Compiling Lib
    [2 of 2] Compiling Paths_helloworld
    Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0..
    Building executable 'helloworld-exe' for helloworld-0.1.0.0..
    [1 of 2] Compiling Main
    [2 of 2] Compiling Paths_helloworld
    [3 of 3] Linking .stack-work\dist\effaccc7\build\helloworld-exe\helloworld-exe.exe
    helloworld> copy/register
    Installing library in ...\helloworld\.stack-work\install\c8c71a24\lib\x86_64-windows-ghc-9.6.6\helloworld-0.1.0.0-KFyX8zLxDvzLZURq3JaCVX
    Installing executable helloworld-exe in ...\helloworld\.stack-work\install\c8c71a24\bin
    Registering library for helloworld-0.1.0.0..
    ~~~

    On Windows, Stack uses hashes of certain information to keep paths short.

## The `stack exec` command

Looking closely at the output of the previous command, you can see that it built
both a library called `helloworld` and an executable called `helloworld-exe` (on
Windows, `helloworld-exe.exe`). We'll explain more in the next section, but, for
now, just notice that the executables are installed in a location in our
project's `.stack-work` directory.

Now, let's use the [`stack exec`](../commands/exec_command.md) command to run
our executable. We command:

~~~text
stack exec helloworld-exe
~~~

and the output is just:
~~~text
someFunc
~~~

??? question "Why is the output just `someFunc`?"

    The code in the `new-template` project template is very simple. The package
    has a Haskell module `Lib`:
    ~~~haskell
    module Lib
        ( someFunc
        ) where

    someFunc :: IO ()
    someFunc = putStrLn "someFunc"
    ~~~

    and a Haskell module `Main`:
    ~~~haskell
    module Main (main) where

    import Lib

    main :: IO ()
    main = someFunc
    ~~~

    `putStrLn "someFunc"` is an action that, when executed, outputs the string
    `someFunc` to the standard output channel.

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

## The `stack test` command

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
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/21.yaml
packages:
- .
~~~

The value of the [`resolver`](../configure/yaml/project.md#resolver) key tells
Stack *how* to build your package: which GHC version to use, versions of package
dependencies, and so on. Our value here says to use
[LTS Haskell 22.21](https://www.stackage.org/lts-22.21), which implies GHC 9.6.5
(which is why `stack build` installs that version of GHC if it is not already
available to Stack). There are a number of values you can use for `resolver`,
which we'll cover later.

The value of the `packages` key tells Stack which project packages, located
locally, to build. In our simple example, we have only a single project package,
located in the same directory, so '`.`' suffices. However, Stack has powerful
support for multi-package projects, which we'll elaborate on as this guide
progresses.

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
Cabal User Guide is the definitive reference for the
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
