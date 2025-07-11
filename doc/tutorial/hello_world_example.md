<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 1. A Hello World example

With Stack installed, let us create a new project and walk through the most
common Stack commands.

In this guide, unless stated otherwise, the working directory is the project's
root directory.

## The `stack new` command

We will start off with the [`stack new`](../commands/new_command.md) command to
create a new *project* from a project template. We will use the `new-template`
project template. This template is used by default, but in our example we will
refer to it expressly.

That template will create a project with a package of the same name. So, we need
to pick a name for the project that is a valid package name. We will call our
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

Next, we will run the most important Stack command,
[`stack build`](../commands/build_command.md). We command:

~~~text
stack build
~~~

Stack needs a version of GHC and, on Windows, a version of MSYS2, in order to
build your project. Stack will discover that you are missing it and will install
it for you.

You will get intermediate download percentage statistics while the download is
occurring. This command may take some time, depending on download speeds.

??? question "Where is the Stack-supplied GHC located?"

    You can use the [`stack path`](../commands/path_command.md) command for path
    information. To identify where GHC is installed, command:

    === "Unix-like"

        ~~~text
        stack exec -- which ghc
        /home/<user_name>/.stack/programs/x86_64-linux/ghc-9.6.5/bin/ghc
        ~~~

    === "Windows"

        ~~~text
        stack exec -- where.exe ghc
        C:\Users\<user_name>\AppData\Local\Programs\stack\x86_64-windows\ghc-9.6.5\bin\ghc.exe
        ~~~

    As you can see from that path, the installation is placed to not interfere
    with any other GHC installation, whether system-wide or different GHC
    versions installed by Stack.

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
    helloworld> build (lib + exe) with ghc-9.8.4
    Preprocessing library for helloworld-0.1.0.0..
    Building library for helloworld-0.1.0.0..
    [1 of 2] Compiling Lib
    [2 of 2] Compiling Paths_helloworld
    Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0..
    Building executable 'helloworld-exe' for helloworld-0.1.0.0..
    [1 of 2] Compiling Main
    [2 of 2] Compiling Paths_helloworld
    [3 of 3] Linking .stack-work/dist/x86_64-linux-tinfo6/ghc-9.8.4/build/helloworld-exe/helloworld-exe
    helloworld> copy/register
    Installing library in .../helloworld/.stack-work/install/x86_64-linux-tinfo6/a2caceceda039eb4f791856f85a68f9582d4daf3d0527344693ff3d1fcd92ba4/9.6.6/lib/x86_64-linux-ghc-9.8.4/helloworld-0.1.0.0-KFyX8zLxDvzLZURq3JaCVX
    Installing executable helloworld-exe in .../helloworld/.stack-work/install/x86_64-linux-tinfo6/a2caceceda039eb4f791856f85a68f9582d4daf3d0527344693ff3d1fcd92ba4/9.6.6/bin
    Registering library for helloworld-0.1.0.0..
    ~~~

=== "Windows"

    ~~~text
    ...
    helloworld> configure (lib + exe)
    Configuring helloworld-0.1.0.0...
    helloworld> build (lib + exe) with ghc-9.8.4
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
    Installing library in ...\helloworld\.stack-work\install\c8c71a24\lib\x86_64-windows-ghc-9.8.4\helloworld-0.1.0.0-KFyX8zLxDvzLZURq3JaCVX
    Installing executable helloworld-exe in ...\helloworld\.stack-work\install\c8c71a24\bin
    Registering library for helloworld-0.1.0.0..
    ~~~

    On Windows, Stack uses hashes of certain information to keep paths short.

Stack aims not to rebuild unnecessarily. If we command `stack build` a second
time, nothing happens.

## The `stack exec` command

The output of the previous command has three main steps. You can see, from the
first two steps, that a library (lib) and an executable (exe) are being built
and that the final step involved the installation of an executable named
`helloworld-exe` (on Windows, the file is `helloworld-exe.exe`) (extract):

~~~text
helloworld> configure (lib + exe)
...
helloworld> build (lib + exe) with ghc-9.8.4
...
helloworld> copy/register
...
Installing executable helloworld-exe in .../helloworld/.stack-work/.../bin
~~~

The executable is installed in a location in the project's `.stack-work`
directory.

Now, let us use the [`stack exec`](../commands/exec_command.md) command to run
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
to find `helloworld-exe` even though it is not on the PATH outside of that
environment.

??? question "How I can find the PATH used in the Stack environment?"

    Command `stack path --bin-path` to see the PATH in the Stack environment.

!!! info

    On Windows, the Stack environment includes the `\mingw64\bin`, `\usr\bin`
    and `\usr\local\bin` directories of the Stack-supplied MSYS2. If your
    executable depends on files (for example, dynamic-link libraries) in those
    directories and you want ro run it outside of the Stack environment, you
    will need to ensure copies of those files are on the PATH.

## The `stack test` command

Finally, like all good software, `helloworld` actually has a test suite.

Let us run it with the [`stack test`](../commands/test_command.md) command. We
command:

~~~text
stack test
~~~

The start of the output should look similar to this:

=== "Unix-like"

    ~~~text
    helloworld-0.1.0.0: unregistering (components added: test:helloworld-test)
    helloworld> configure (lib + exe + test)
    Configuring helloworld-0.1.0.0...
    helloworld> build (lib + exe + test) with ghc-9.8.4
    Preprocessing library for helloworld-0.1.0.0..
    Building library for helloworld-0.1.0.0..
    Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0..
    Building test suite 'helloworld-test' for helloworld-0.1.0.0..
    [1 of 2] Compiling Main
    [2 of 2] Compiling Paths_helloworld
    [3 of 3] Linking .stack-work/dist/x86_64-linux-tinfo6/ghc-9.8.4/build/helloworld-test/helloworld-test
    Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0..
    Building executable 'helloworld-exe' for helloworld-0.1.0.0..
    helloworld> copy/register
    Installing library in .../helloworld/.stack-work/install/x86_64-linux-tinfo6/a2caceceda039eb4f791856f85a68f9582d4daf3d0527344693ff3d1fcd92ba4/9.6.6/lib/x86_64-linux-ghc-9.8.4/helloworld-0.1.0.0-KFyX8zLxDvzLZURq3JaCVX
    Installing executable helloworld-exe in .../helloworld/.stack-work/install/x86_64-linux-tinfo6/a2caceceda039eb4f791856f85a68f9582d4daf3d0527344693ff3d1fcd92ba4/9.6.6/bin
    Registering library for helloworld-0.1.0.0..
    ~~~

=== "Windows"

    ~~~text
    helloworld-0.1.0.0: unregistering (components added: test:helloworld-test)
    helloworld> configure (lib + exe + test)
    Configuring helloworld-0.1.0.0...
    helloworld> build (lib + exe + test) with ghc-9.8.4
    Preprocessing library for helloworld-0.1.0.0..
    Building library for helloworld-0.1.0.0..
    Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0..
    Building test suite 'helloworld-test' for helloworld-0.1.0.0..
    [1 of 2] Compiling Main
    [2 of 2] Compiling Paths_helloworld
    [3 of 3] Linking .stack-work\dist\effaccc7\build\helloworld-test\helloworld-test.exe
    Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0..
    Building executable 'helloworld-exe' for helloworld-0.1.0.0..
    helloworld> copy/register
    Installing library in ...\helloworld\.stack-work\install\0aa166fa\lib\x86_64-windows-ghc-9.8.4\helloworld-0.1.0.0-KFyX8zLxDvzLZURq3JaCVX
    Installing executable helloworld-exe in ...\helloworld\.stack-work\install\0aa166fa\bin
    Registering library for helloworld-0.1.0.0..
    ~~~

Again, Stack does not rebuild unnecessarily. Only the test suite component is
compiled and linked.

The output should then conclude:

~~~text
helloworld> test (suite: helloworld-test)

Test suite not yet implemented



helloworld> Test suite helloworld-test passed
Completed 2 action(s).
~~~

Having build the test suite executable, Stack then automatically runs it.
