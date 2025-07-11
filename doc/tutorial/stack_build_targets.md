  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 8. `stack build` targets

We have not discussed this too much yet, but, in addition to having a number of
synonyms *and* taking a number of options on the command line, the `build`
command *also* takes many arguments. These are parsed in different ways, and can
be used to achieve a high level of flexibility in telling Stack exactly what you
want to build.

We are not going to cover the full generality of these arguments here; instead,
there is documentation covering the full
[build command syntax](../commands/build_command.md). Here, we will just point
out a few different types of arguments:

* You can specify a *package name*, e.g. `stack build vector`.
    * This will attempt to build the `vector` package, whether it is a local
      package, in your extra-deps, in your snapshot, or just available upstream.
      If it is just available upstream but not included in your locals,
      extra-deps, or snapshot, the newest version is automatically promoted to
      an extra-dep.
* You can also give a *package identifier*, which is a package name plus
  version, e.g. `stack build yesod-bin-1.4.14`.
    * This is almost identical to specifying a package name, except it will (1)
      choose the given version instead of latest, and (2) error out if the given
      version conflicts with the version of a project package.
* The most flexibility comes from specifying individual *components*, e.g.
  `stack build helloworld:test:helloworld-test` says "build the test suite
  component named helloworld-test from the helloworld package."
    * In addition to this long form, you can also shorten it by skipping what
      type of component it is, e.g. `stack build helloworld:helloworld-test`, or
      even skip the package name entirely, e.g. `stack build :helloworld-test`.
* Finally, you can specify individual *directories* to build to trigger building
  of any project packages included in those directories or subdirectories.

When you give no specific arguments on the command line (e.g., `stack build`),
it is the same as specifying the names of all of your project packages. If you
just want to build the package for the directory you are currently in, you can
use `stack build .`.

## Components, --test, and --bench

Here is one final important yet subtle point. Consider our `helloworld` package:
it has a library component, an executable `helloworld-exe`, and a test suite
`helloworld-test`. When you run `stack build helloworld`, how does it know which
ones to build? By default, it will build the library (if any) and all of the
executables but ignore the test suites and benchmarks.

This is where the `--test` and `--bench` flags come into play. If you use them,
those components will also be included. So `stack build --test helloworld` will
end up including the helloworld-test component as well.

You can bypass this implicit adding of components by being much more explicit,
and stating the components directly. For example, the following will not build
the `helloworld-exe` executable:

~~~text
stack purge
stack build :helloworld-test
helloworld> configure (lib + test)
Configuring helloworld-0.1.0.0...
helloworld> build (lib + test) with ghc-9.6.5
Preprocessing library for helloworld-0.1.0.0..
Building library for helloworld-0.1.0.0..
[1 of 2] Compiling Lib
[2 of 2] Compiling Paths_helloworld
Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0..
Building test suite 'helloworld-test' for helloworld-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_helloworld
[3 of 3] Linking .stack-work\dist\<hash>\build\helloworld-test\helloworld-test.exe
helloworld> copy/register
Installing library in ...\helloworld\.stack-work\install\...
Registering library for helloworld-0.1.0.0..
helloworld> test (suite: helloworld-test)

Test suite not yet implemented



helloworld> Test suite helloworld-test passed
Completed 2 action(s).
~~~

We first purged our project to clear old results so we know exactly what Stack
is trying to do.

The last line shows that our command also *runs* the test suite it just built.
This may surprise some people who would expect tests to only be run when using
`stack test`, but this design decision is what allows the `stack build` command
to be as composable as it is (as described previously). The same rule applies to
benchmarks. To spell it out completely:

* The `--test` and `--bench` flags simply state which components of a package
  should be built, if no explicit set of components is given
* The default behavior for any test suite or benchmark component which has been
  built is to also run it

You can use the `--no-run-tests` and `--no-run-benchmarks` flags to disable
running of these components. You can also use `--no-rerun-tests` to prevent
running a test suite which has already passed and has not changed.

!!! note

    Stack does not build or run test suites and benchmarks for non-local
    packages. This is done so that a command like `stack test` does not need to
    run 200 test suites!
