  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 1. A Hello World Example

With Stack installed, let's create a new project from a template and walk
through the most common Stack commands.

In this guide, an initial `$` represents the command line prompt. The prompt may
differ in the terminal on your operating system. Unless stated otherwise, the
working directory is the project's root directory.

## The `stack new` command

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
[documentation](../commands/templates_command.md).

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
configuration
[documentation](../configure/yaml/yaml_configuration.md#templates). For further
information about initialisation, see the
[`stack init`](../commands/init_command.md) command documentation. The
`stack new` and `stack init` commands have options and flags in common.

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

## The `stack build` command

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

## The `stack exec` command

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
