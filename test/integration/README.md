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

A more thorough way to run the tests is with

```shell
$ stack test --flag stack:integration-tests stack:test:stack-integration-test
```

Note that this command can take a _long_ time. It's also more thorough
than the quick command given above, as it will run each test with a
clean `STACK_ROOT`.
