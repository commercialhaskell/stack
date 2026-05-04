# Stack Integration Tests

This directory, `tests/integration`, contains integration tests for Stack. Each
directory inside its `tests` subdirectory represents a single test. Each of
those directories has:

* a `Main.hs` file, which provides the script to be run; and
* a `files `directory, providing the working directory the script will be run
  from. (If you have a test that does not require any specific working
  directory, there may be no `files` directory.)

The easiest way to create a new integration test is to use existing examples as
a foundation.

## Form

Most integration tests have a similar form, as follows.

An initial comment in the `Main.hs` file explains what the integration test is
testing. If the test is prompted by a GitHub issue, the commment includes a URL
to that issue. Tests prompted by a GitHub issue are usually named after that
issue.

Packages in tests are usually described by `package.yaml` files. The associated
Cabal file is usually not checked in. Consequently, a `.gitignore` file lists
the Cabal file that is to be ignored.

The most complicated 'generic' test package has the description below. The
`spec-version: 0.36.0` avoids Hpack's legacy syntax and the package version is
`0.0.0` by default.

A main (unnamed) library module is usually named `Lib.hs` in directory `src`. A
private named sublibrary (an internal library) is usually named `Internal.hs` in
directory `int`.

~~~yaml
# package.yaml
spec-version: 0.36.0

name: myPackage

dependencies:
- base

library:
  source-dirs: src
  dependencies:
  - myPackage:internal

internal-libraries:
  internal:
    source-dirs: int
  sublib:
    visibility: public
    source-dirs: sub
    dependencies:
    - myPackage:internal

executables:
  myExe:
    source-dirs: app
    main: Main.hs
    dependencies:
    - myPackage
    - myPackage:{internal, sublib}

tests:
  test:
    source-dirs: test
    main: Main.hs
    dependencies:
    - myPackage
    - myPackage:{internal, sublib}

benchmarks:
  bench:
    source-dirs: bench
    main: Main.hs
    dependencies:
    - myPackage
    - myPackage:{internal, sublib}
~~~

Usually, a test package does not need a `Setup.hs` file.

Many tests require only a compiler version snapshot.

If an extra-dep is required, `ansi-missiles-0.3` is often used.

## Running

One simple way to run a single test is:

* Change into the `files` directory
* Run the command `stack runghc -- -i"../../../lib" ../Main.hs`

A more thorough way to run the tests is with command:

~~~text
stack build --flag stack:integration-tests stack --exec stack-integration-test
~~~

Note that this command can take a _long_ time. It is also more thorough
than the quick command given above, as it will run each test with a
clean `STACK_ROOT`.

On Linux, the `stack-integration-test` executable uses the `lld` linker and
expects it to be on the PATH. The integration tests complete significantly
quicker with `lld` than with the `ld.bfd` linker.

## Helper scripts

There are two helper scripts in this directory. Note that these may
not always work as anticipated, since some of the tests expect a clean
`STACK_ROOT`, and these scripts do not set that up.

* `run-sort-tests.sh` will run all of the tests in the `tests`
  directory, and move the successful ones into `tests-success`, and
  the failing ones into `tests-fail`. It will keep the logs of failing
  tests in `logs`.
* `run-single-test.sh` takes a single argument (the name of a test),
  and runs just that test.
