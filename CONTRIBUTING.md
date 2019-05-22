# Contributors Guide

## Bug Reports

Please [open an issue](https://github.com/commercialhaskell/stack/issues/new)
and use the provided template to include all necessary details.

The more detailed your report, the faster it can be resolved and will ensure it
is resolved in the right way. Once your bug has been resolved, the responsible
person will tag the issue as _Needs confirmation_ and assign the issue back to
you. Once you have tested and confirmed that the issue is resolved, close the
issue. If you are not a member of the project, you will be asked for
confirmation and we will close it.


## Contributing Overview

Contributions to The Haskell Tool Stack are most welcome. To get started, we 
suggest that you get the source code of the project onto your local machine by 
following these steps (this assumes that you have already installed a version 
of stack and have `git` installed on your machine).

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

Here is a one-line command that will execute steps 1 through 3 at once.
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

This allows you to set a special stack root (instead of `~/.stack/` or, on
Windows, `%LOCALAPPDATA%\Programs\stack`) and to target your commands at a
particular `stack.yaml` instead of the one found in the current directory.

If you want to contribute to documentation, please take a look at the 
[Contributing to Documentation](#contributing-to-documentation) section below. 
If you want to contribute to the source code, please take a look at the 
[Contributing to Code](#contributing-to-code) section below.


## Contributing to Documentation

[1]: https://github.com/commercialhaskell/stack/tree/stable

If you would like to help with documentation, please note that for most cases
the Wiki has been deprecated in favor of markdown files placed in a new `/doc`
subdirectory of the repository itself. Please create a new branch off of the 
project's [stable branch][1] and make your changes/additions on the new 
branch. Once you are ready to submit your changes/additions, please submit a 
pull request from your new branch to the project's [stable branch][1]. For 
more information about pull requests, take a look at GitHub's overview of 
[using pull requests](https://help.github.com/articles/using-pull-requests/).

The documentation is rendered on [haskellstack.org](http://haskellstack.org) by
readthedocs.org using Sphinx and CommonMark. Since links and formatting vary
from GitHub Flavored Markdown, please check the documentation there before 
submitting a pull request to fix those.

If your changes move or rename files, or subsume Wiki content, please continue
to leave a file/page in the old location temporarily, in addition to the new
location. This will allow users time to update any shared links to the old
location. Please also update any links in other files, or on the Wiki, to point
to the new file location.


## Contributing to Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve `stack`, pull requests are most welcome. It's a good idea to
[submit an issue](https://github.com/commercialhaskell/stack/issues/new) to
discuss the change before plowing into writing code.

If you'd like to help out but aren't sure what to work on, look for issues with
the
[awaiting pull request](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pull+request%22)
label. Issues that are suitable for newcomers to the codebase have the
[newcomer friendly](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pull+request%22+label%3a%22newcomer+friendly%22)
label. Best to post a comment to the issue before you start work, in case anyone
has already started.

Please include a
[ChangeLog](https://github.com/commercialhaskell/stack/blob/master/ChangeLog.md)
entry and
[documentation](https://github.com/commercialhaskell/stack/tree/master/doc/)
updates with your pull request.

## Code Quality

The Stack projects uses [HLint](https://github.com/ndmitchell/hlint) as a code
quality tool.

Note that stack contributors need not dogmatically follow the suggested hints
but are encouraged to debate their usefulness. If you find a hint is not useful
and detracts from readability, consider marking it in the [configuration
file](https://github.com/commercialhaskell/stack/blob/master/.hlint.yaml) to
be ignored. Please refer to the [HLint manual](https://github.com/ndmitchell/hlint#readme)
for configuration syntax.

Quoting [@mgsloan](https://github.com/commercialhaskell/stack/pulls?utf8=%E2%9C%93&q=is%3Apr%20author%3Amgsloan):

> We are optimizing for code clarity, not code concision or what HLint thinks.

You can install HLint with stack. You might want to install it in the global
project in case you run into dependency conflicts. HLint can report hints in
your favourite text editor. Refer to the HLint repository for more details.

To install:

```
stack install hlint
```

Once installed, you can check your changes with:

```
$ ./etc/scripts/hlint.sh
```

## Testing

The Stack code has both unit tests and integration tests. Integration tests can
be found in the [test/integration](https://github.com/commercialhaskell/stack/tree/master/test/integration)
folder and unit tests, in the [src/test](https://github.com/commercialhaskell/stack/tree/master/src/test)
folder. Tests are written using the [Hspec](https://hspec.github.io/) framework. In
order to run the full test suite, you can simply do:

```bash
$ stack test
```

The `--file-watch` is a very useful option to get quick feedback. However,
running the entire test suite after each file change will slow you down. You'll
need to specify which test suite (unit test or integration) and pass arguments
to specify which module you'd specifically like to run to get quick feedback. A
description of this follows below.

### Working with Unit Tests

If you would like to run the unit tests on their own, you can:

```bash
$ stack test stack:stack-test
```

Running an individual module works like this:

```bash
$ stack test stack:stack-test --ta "-m <PATTERN>"
```

Where `<PATTERN>` is the name of the module without `Spec.hs`.

You may also load tests into GHCi and run them with:

```bash
$ stack ghci stack:stack-test --only-main
# GHCi starting up output ...
> :main -m "<PATTERN>"
```

Where again, `<PATTERN>` is the name of the module without `Spec.hs`.

### Working with Integration Tests

Running the integration tests is a little involved, you'll need to:

```bash
$ stack test stack:stack-integration-test --flag stack:integration-tests
```

Running an individual module works like this:

```bash
$ stack test stack:stack-integration-test --flag stack:integration-tests --ta "-m <PATTERN>"
```

Where `<PATTERN>` is the name of the folder listed in the
[test/integration/tests/](https://github.com/commercialhaskell/stack/tree/master/test/integration/tests)
folder.

You may also achieve this through GHCi with:

```bash
$ stack ghci stack:stack-integration-test
# GHCi starting up output ...
> :main -m "<PATTERN>"
```

Where again, `<PATTERN>` is the name of the folder listed in the
[test/integration/tests/](https://github.com/commercialhaskell/stack/tree/master/test/integration/tests)
folder.


## Slack channel

If you're making deep changes and real-time communcation with the Stack team
would be helpful, we have a `#stack-collaborators` Slack channel.  Please
contact [@borsboom](https://github.com/borsboom) (manny@fpcomplete.com) or
[@snoyberg](https://github.com/snoyberg) (michael@fpcomplete.com) for an
invite.
