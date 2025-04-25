# Stack Integration Tests

This directory contains a bunch of integration tests for Stack. Each
directory inside the `tests` subdirectory represents a single
test. Each of those directories has:

* A `Main.hs` file, which provides the script to be run
* A `files `directory, providing the working directory the script will
  be run from. (If you have a test that doesn't require any specific
  working directory, there may be no `files` directory.)

It would be great to expand this file into a full tutorial, but for
now, the easiest way to get started with writing an integration test
is to copy an existing example.

## Running

One simple way to run a single test is:

* Change into the `files` directory
* Run the command `stack runghc -- -i../../../lib ../Main.hs`

A more thorough way to run the tests is with command:

~~~text
stack build --flag stack:integration-tests stack --exec stack-integration-test
~~~

Note that this command can take a _long_ time. It's also more thorough
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
