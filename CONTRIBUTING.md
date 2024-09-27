# Contributors Guide

Thank you for considering contributing to the maintenance or development of
Stack, or otherwise supporting users of Stack! We hope that the following
information will encourage and assist you. We start with some advice about
Stack's goals and governance, and approach to supporting users.

## Stack's goals

Stack's current goals are:

* To provide easy to use tooling for Haskell development
* To provide complete support for at least the following three development
  environments: Linux, macOS, and Windows
* To address the needs of industrial users, open source maintainers, and other
  people
* To focus on the 'curated package set' use case
* To prioritize reproducible build plans

The goals above are not set in stone. However, any major changes to them
should involve significant public discussion and a public vote by the Stack
maintainer team.

## Stack's governance

People involved in maintaining or developing Stack with rights to make commits
to the repository can be classified into two groups: 'committers' and
'maintainers'.

### Stack's committers

We encourages a wide range of people to be granted rights to make commits to the
repository.

People are encouraged to take initiative to make non-controversial
changes, such as documentation improvements, bug fixes, performance
improvements, and feature enhancements.

Maintainers should be included in discussions of controversial changes and
tricky code changes.

Our general approach is **"it's easier to ask forgiveness than permission"**. If
there is ever a bad change, it can always be rolled back.

### Stack's maintainers

Stack's maintainers are long-term contributors to the project. Michael Snoyman
(@snoyberg) was the founder of Stack, and its initial maintainer - and he has
added others. Michael's current interests and priorities mean that he is no
longer actively involved in adding new features to Stack.

Maintainers are recognized for their contributions including:

* Direct code contribution
* Review of pull requests
* Interactions on the GitHub issue tracker
* Documentation management
* External support - for example, hosting or training

The maintainer team make certain decisions when that is necessary, specifically:

* How to proceed, if there is disagreement on how to do so on a specific topic
* Whether to add or remove (see further below) a maintainer

Generally, maintainers are only removed due to non-participation or actions
unhealthy to the project. Removal due to non-participation is not a punishment,
simply a recognition that maintainership is for active participants only.

We hope that removal due to unhealthy actions will never be necessary, but would
include protection for cases of:

* Disruptive behavior in public channels related to Stack
* Impairing the codebase through bad commits/merges

Like committers, maintainers are broadly encouraged to make autonomous
decisions. Each maintainer is empowered to make a unilateral decision. However,
maintainers should favor getting consensus first if:

* They are uncertain what is the best course of action
* They anticipate that other maintainers or users of Stack will disagree on the
  decision

## Stack's support

A large part of the general discussion around Stack is on support-related
topics, and that is reflected in the current issue tracker content. Assistance
in responding to such matters is greatly appreciated.

While support-related matters can be posted here as an 'issue', we encourage the
use of other forums, in particular the
[Haskell Community](https://discourse.haskell.org/) forum. See its 'Learn'
category. We also recommend that forum for general discussions about Stack's
current or desired features.

Stack is also discussed:

* in the Haskell
  [Stack and Stackage](https://matrix.to/#/#haskell-stack:matrix.org) room
  (address `#haskell-stack:matrix.org`) on [Matrix](https://matrix.org/); and

* on Reddit's [Haskell community](https://www.reddit.com/r/haskell/).

We encourage use of those other forums because support-related discussions can
clog up the issue tracker and make it more difficult to maintain the project.
People needing support may also get a faster and fuller response on other
forums.

Additions to the issue tracker are better suited to concrete feature proposals,
bug reports, and other code base discussions (for example, refactorings).

## Bug Reports

Please [open an issue](https://github.com/commercialhaskell/stack/issues/new)
and use the provided template to include all necessary details.

The more detailed your report, the faster it can be resolved and will ensure it
is resolved in the right way. Once your bug has been resolved, the responsible
person will tag the issue as _Needs confirmation_ and assign the issue back to
you. Once you have tested and confirmed that the issue is resolved, close the
issue. If you are not a member of the project, you will be asked for
confirmation and we will close it.

## Documentation

Consistent with its goal of being easy to use, Stack aims to maintain a high
quality of in-tool and online documentation.

The in-tool documentation includes the output when the `--help` flag is
specified and the content of Stack's warning and error messages.

When drafting documentation it is helpful to have in mind the intended reader
and what they are assumed to know, and not know, already. In that regard,
documentation should aim to meet, at least, the needs of a person who is about
to begin to study computing as an undergraduate but who has not previously
coded using Haskell. That person may be familiar with one popular operating
system but may not be familiar with others.

The files which make up Stack's online documentation are located in the `doc`
directory of the repository. They are formatted in the
[Markdown syntax](https://daringfireball.net/projects/markdown/), with some
extensions.

Those files are rendered on [haskellstack.org](http://haskellstack.org) by
[Read the Docs](https://readthedocs.org/) using
[MkDocs](https://www.mkdocs.org/) and the
[Material for MkDocs](https://squidfunk.github.io/mkdocs-material/) theme. The
`stable` branch of the repository provides the 'stable' version of the online
documentation. The `master` branch provides the 'latest' version of the
documentation.

The 'stable' version of the online documentation is intended to be applicable to
the latest released version of Stack. If you would like to help with that
documentation, please submit a
[pull request](https://help.github.com/articles/using-pull-requests/) with your
changes/additions based off the
[stable branch](https://github.com/commercialhaskell/stack/tree/stable).

The Markdown files are organised into the navigation menu (the table of
contents) in the file `mkdocs.yml`, the configuration file for MkDocs. The
description of a file in the menu can differ from the file's name. The
navigation menu allows files to be organised in a hierarchy. Currently, up to
three levels are used. The top level is:

* **Welcome!:** The introduction to Stack. This page aims to be no longer than
  necessary but also to not assume much existing knowledge on the part of the
  reader. It provides a 'quick start' guide to getting and using Stack.
* **How to get & use Stack:** This includes Stack's user's guide, answers to
  frequently asked questions, and more thorough explanations of aspects of
  Stack. The user's guide is divided into two parts. The first part is
  'introductory', and has the style of a tutorial. The second part is
  'advanced', and has more of a reference style.
* **How Stack works (advanced):** Many users will not need to consult this
  advanced documentation.
* **Stack's code (advanced):** Other information useful to people contributing
  to, or maintaining, Stack's code, documentation, and other files.
* **Signing key:** How Stack's released executables are signed.
* **Glossary:** A glossary of terms used throughout Stack's in-tool and online
  documentation. We aim to describe the same things in the same way in different
  places.
* **Version history:** The log of changes to Stack between versions.

The specific versions of the online documentation (eg `v: v2.9.1`) are generated
from the content of files at the point in the repository's history specified by
the corresponding release tag. Consequently, that content is fixed once
released.

If the names of Markdown files do not change between versions, then people can
use the flyout on the online documentation to move between different versions of
the same page. For that reason, the names of new Markdown files should be chosen
with care and existing Markdown files should not be deleted or renamed without
due consideration of the consequences.

The Markdown syntax supported by MkDocs and the Material for MkDocs theme can
differ from the GitHub Flavored Markdown ([GFM](https://github.github.com/gfm/))
supported for content on GitHub.com. Please refer to the
[MkDocs documentation](https://www.mkdocs.org/user-guide/writing-your-docs/#writing-with-markdown)
and the
[Material for MkDocs reference](https://squidfunk.github.io/mkdocs-material/reference/)
to ensure your pull request will achieve the desired rendering.

The extensions to the basic Markdown syntax used are set out in `mkdocs.yml` and
include:

* admonitions
* code blocks, with syntax highlighting provided by
  [Pygments](https://pygments.org/)
* content tabs, which can be nested
* icons and emojis

The files in the `doc` directory of the repository include two symbolic links
(symlinks), `ChangeLog.md` and `CONTRIBUTING.md`. Users of Git on Windows should
be aware of its approach to symbolic links. See the
[Git for Windows Wiki](https://github.com/git-for-windows/git/wiki/Symbolic-Links).
If `git config --show-scope --show-origin core.symlinks` is `false` in a local
repository on Windows, then the files will be checked out as small plain files
that contain the link text  See the
[Git documentation](https://git-scm.com/docs/git-config#Documentation/git-config.txt-coresymlinks).

The online documentation can be previewed using the `mkdocs` tool, as described
in [Getting Started with MkDocs](https://www.mkdocs.org/getting-started/). The
prerequisites are:

*   [Python](https://www.python.org/); and
*   the required Python packages set out in `doc/requirements.txt`. They can be
    installed using Python's package manager `pip` with:

    ~~~text
    pip install --requirement doc/requirements.txt
    ~~~

Once the required version of `mkdocs` is installed, command `mkdocs serve` in
the same directory as the `mkdocs.yml` file to start a web server. The command
will, eventually, output the URL at which the documentation is being served.

Command `mkdocs build` to build the documentation.

=== "Windows"

    With the correct prerequisites (see further below), users of the `make` tool
    in the Stack-supplied MSYS2 environment can automate some of these steps
    from Stack's project directory with:

    * preview: `stack exec -- make docs-serve`; and
    * build: `stack exec -- make _site/index.html`.

    However, Windows and the Stack-supplied MSYS2 environment do not come with
    Python or `make` by default. Further, Python on Windows does not use the
    `python3` command (used on Unix-like operating systems) to invoke Python.
    Further still, in the MSYS2 environment, development versions of packages
    `libxml2` and `libxslt` are necessary dependencies. Consequently, the
    automation requires the following command to install requirements into the
    the MSYS2 environment:

    ~~~text
    stack exec -- pacman --sync python make libxml2-devel libxslt-devel
    ~~~

    !!! note

        If the automation fails before the `mkdocs` tool etc is installed, the
        directory `.python-doc-virtualenv` created by the automation will need
        to be deleted before the automation will work again.

    For most users, the automation will be less convenient than simply using the
    `mkdocs serve` command directly.

=== "Unix-like"

    With `python3` and `make` available on the PATH, users of the `make` tool
    can automate some of these steps from Stack's project directory with:

    * preview: `make docs-serve`; and
    * build: `make _site/index.html`.

## Error messages

Stack catches exceptions thrown by its dependencies or by Stack itself in
`Stack.main`. In addition to exceptions that halt Stack's execution, Stack logs
certain other matters as 'errors'.

To support the Haskell Foundation's
[Haskell Error Index](https://errors.haskell.org/) initiative, all Stack
error messages generated by Stack itself should have a unique initial line:

~~~text
Error: [S-nnnn]
~~~

where `nnnn` is a four-digit number in the range 1000 to 9999.

If you create a new Stack error, select a number using a random number generator
(see, for example, [RANDOM.ORG](https://www.random.org/)) and check that number
is not already in use in Stack's code. If it is, pick another until the number
is unique.

All exceptions generated by Stack itself are implemented using data constructors
of closed sum types. Typically, there is one such type for each module that
exports functions that throw exceptions. This type and the related `instance`
definitions are usually located at the top of the relevant module.

Stack supports two types of exceptions: 'pretty' exceptions that are instances
of class `RIO.PrettyPrint.Pretty`, which provides `pretty :: e -> StyleDoc`, and
thrown as expressions of type `RIO.PrettyPrint.PrettyException.PrettyException`;
and other 'plain' exceptions that are simply instances of class
`Control.Exception.Exception` and, hence, instances of class `Show`. These types
and classes are re-exported by `Stack.Prelude`.

Stack throws exceptions in parts of the code that should, in principle, be
unreachable. The functions `Stack.Prelude.bugReport` and
`Stack.Prelude.bugPrettyReport` are used to give the messages a consistent
format. The names of the data constructors for those exceptions usually end in
`Bug`.

In a few cases, Stack may throw an exception in 'pure' code. The function
`RIO.impureThrow :: Exception e => e -> a`, re-exported by `Stack.Prelude`, is
used for that purpose.

## Code

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

## Backwards Compatability

The Stack package provides a library and an executable (`stack`) that depends on
the library. The library is intended for use only by the executable.

Consequently, the Stack package does not need to, and does not, strive for the
compatibility with a range of versions of GHC that a library package (such as
`pantry`) would seek.

Stack aims to depend on well-known packages. The specific versions on which it
depends at any time are specified by `package.yaml` and `stack.yaml`. It does
not aim to be compatible with more than one version of the `Cabal` package at
any time. At the time of writing (september 2024) the package versions are
primarily ones in Stackage snapshot LTS Haskell 22.28 (for GHC 9.6.6), together
with extra-deps to depend on the latest version of `hpack`, `pantry`, `tar`,
`tls` and `crypton-connection`.

A Stack executable makes use of Cabal (the library) through a small 'Setup'
executable that it compiles from Haskell source code. The executable compiles
that code with a dependency on the version of Cabal that ships with the
specified GHC compiler. Each release of Stack will normally aim to support all
versions of GHC and the Cabal package in Stackage snapshots published within
seven years of the release. For example, snapshot LTS Haskell 10.0, published on
19 December 2017, was the first LTS Haskell snapshot to provide GHC 8.2.2 which
comes with `base-4.10.1.0` and `Cabal-2.0.1.1`. Normally, until, at least,
19 December 2024, Stack releases would aim to support the immediate
predecessor, GHC 8.0.2 and `base-4.9.1.0`, `Cabal-1.24.2.0` and Haddock 2.17.4.
However, the next version of Stack will drop support for versions of Cabal
before 2.2. `Cabal-2.2.0.0` was released with GHC 8.4.1 on 8 March 2018.

When a version of the Stack executable actually ceases to support a version of
GHC and `Cabal`, that should be recorded in Stack's
[ChangeLog](https://github.com/commercialhaskell/stack/blob/master/ChangeLog.md).

## Code Quality

The Stack project uses [yamllint](https://github.com/adrienverge/yamllint) as a
YAML file quality tool and [HLint](https://github.com/ndmitchell/hlint) as a
code quality tool.

### Linting of YAML files

The yamllint configuration extends the tools default and is set out in
`.yamllint.yaml`. In particular, indentation is set at 2 spaces and `- ` in
sequences is treated as part of the indentation.

### Linting of Haskell source code

The HLint configurations is set out in `.hlint.yaml`.

Stack contributors need not follow dogmatically the suggested HLint hints but
are encouraged to debate their usefulness. If you find a HLint hint is not
useful and detracts from readability of code, consider marking it in the
[configuration file](https://github.com/commercialhaskell/stack/blob/master/.hlint.yaml)
to be ignored. Please refer to the
[HLint manual](https://github.com/ndmitchell/hlint#readme)
for configuration syntax.

Quoting
[@mgsloan](https://github.com/commercialhaskell/stack/pulls?utf8=%E2%9C%93&q=is%3Apr%20author%3Amgsloan):

> We are optimizing for code clarity, not code concision or what HLint thinks.

You can install HLint with Stack. You might want to install it in the global
project in case you run into dependency conflicts. HLint can report hints in
your favourite text editor. Refer to the HLint repository for more details.

To install, command:

~~~text
stack install hlint
~~~

Once installed, you can check your changes with command:

~~~text
stack exec -- sh ./etc/scripts/hlint.sh
~~~

## Code syntax

Stack makes use of GHC's `GHC2021` collection of language extensions, which is
set using the `language` key in the `package.yaml` file.

Stack makes use of single-constructor types where the constructor has a large
number of fields. Some of those fields have similar types, and so on. Given
that, Stack makes use of `OverloadedRecordDot`, introduced in GHC 9.2.1. It also
makes use of `NoFieldSelectors`, also introduced in GHC 9.2.1, and, where
necessary, `DuplicateRecordFields`. Together, these language extensions enable
the removal from the names of fields of the prefixes that were used historically
to indicate the type and make field names unique. This is because the names of
fields no longer need to be unique in situations where the intended field is
unambiguous. This allows for a terser syntax without loss of expressiveness.
For example:

~~~haskell
let cliTargets = (boptsCLITargets . bcoBuildOptsCLI) bco
~~~

can become:

~~~haskell
let cliTargets = bco.buildOptsCLI.targets
~~~

The intended field is unambiguous in almost all cases. In the case of a few
record updates it is ambiguous. The name of the field needs to be qualified in
those cases. For example:

~~~haskell
import qualified  Stack.Types.Build as ConfigCache ( ConfigCache (..) )
...
let ignoreComponents :: ConfigCache -> ConfigCache
    ignoreComponents cc = cc { ConfigCache.components = Set.empty }
~~~

## Code Style

A single code style is not applied consistently to Stack's code and Stack is not
Procrustean about matters of style. Rules of thumb, however, are:

* keep pull requests that simply reformat code separate from those that make
  other changes to code; and
* when making changes to code other than reformatting, follow the existing style
  of the function(s) or module(s) in question.

That said, the following may help:

* Stack's code generally avoids the use of C preprocessor (CPP) directives.
  Windows and non-Windows code is separated in separate source code directories
  and distinguished in Stack's Cabal file. `Stack.Constants.osIsWindows :: Bool`
  is provided. Multi-line strings are generally formatted on the assumption that
  GHC's `CPP` language pragma is not being used.
* Language pragmas usually start with `NoImplictPrelude`, where applicable, and
  then all others are listed alphabetically. The closing `#-}` are aligned, for
  purely aesthetic reasons.
* Stack is compiled with GHC's `-Wall` enabled, which includes `-Wtabs` (no tabs
  in source code). Most modules are based on two spaces (with one space for a
  `where`) for indentation but older and larger modules are still based on four
  spaces.
* Stack's code and documentation tends to be based on lines of no more than 80
  characters or, if longer, no longer than necessary.
* Stack uses export lists.
* Stack's imports are listed alphabetically, including `Stack.Prelude`, where
  applicable. The module names are left aligned, with space left for `qualified`
  where it is absent.
* Stack's code is sufficiently stable that explict import lists can sensibly be
  used. The exception is the import of `Stack.Prelude`. Not all modules have
  comprehensive explicit import lists.
* Short explicit import lists follow the module name. Longer lists start on the
  line below the module name. Spaces are used to separate listed items from
  their enclosing parentheses.
* As noted above, the types used to implement Stack's exceptions and the related
 `instance` definitions are usually located at the top of the relevant module.
* In function type signatures, the `::` is kept on the same line as the
  function's name. This format is Haskell syntax highlighter-friendly.
* If `where` is used, the declarations follow on a separate line.

## Testing

The Stack code has both unit tests and integration tests.

### Working with Unit Tests

Unit tests can be found in the
[tests/unit](https://github.com/commercialhaskell/stack/tree/master/tests/unit)
directory. Tests are written using the [Hspec](https://hspec.github.io/)
framework. In order to run the full test suite, you can simply command:

~~~text
stack test
~~~

The `--file-watch` is a very useful option to get quick feedback. However,
running the entire test suite after each file change will slow you down. You'll
need to specify which test suite (unit test or integration) and pass arguments
to specify which module you'd specifically like to run to get quick feedback. A
description of this follows below.


If you would like to run the unit tests on their own, you can command:

~~~text
stack test stack:stack-unit-test
~~~

Running an individual module works with a command like this:

~~~text
stack test stack:stack-unit-test --ta "-m <PATTERN>"
~~~

Where `<PATTERN>` is the name of the module without `Spec.hs`.

You may also load tests into GHCi and run them with these command:

~~~text
stack ghci stack:stack-unit-test --only-main
# GHCi starting up output ...
> :main -m "<PATTERN>"
~~~

Where again, `<PATTERN>` is the name of the module without `Spec.hs`.

### Working with Integration Tests

Integration tests can be found in the
[tests/integration](https://github.com/commercialhaskell/stack/tree/master/tests/integration)
folder.

Running the integration tests is a little involved, you'll need to command:

~~~text
stack build --flag stack:integration-tests stack --exec stack-integration-test
~~~

Running an individual module works with a command like this:

~~~text
stack build --flag stack:integration-tests stack --exec "stack-integration-test -m <PATTERN>"
~~~

Where `<PATTERN>` is the name of the folder listed in the
[test/integration/tests/](https://github.com/commercialhaskell/stack/tree/master/test/integration/tests)
directory.

You may also achieve this through GHCi with this command:

~~~text
stack ghci stack:stack-integration-test
# GHCi starting up output ...
> :main -m "<PATTERN>"
~~~

Where again, `<PATTERN>` is the name of the folder listed in the
[test/integration/tests/](https://github.com/commercialhaskell/stack/tree/master/test/integration/tests)
directory.

You can disable a few integration tests through the -n option :

~~~text
stack build --flag stack:integration-tests stack --exec "stack-integration-test -n <PATTERN1> -n <PATTERN2>"
~~~

To disable folders named after `<PATTERN1>` and `<PATTERN2>`
It's especially useful when some tests are taking a while to complete.

## Continuous integration (CI)

We use [GitHub Actions](https://docs.github.com/en/actions) to do CI on Stack.
The configuration of the workflows is in the YAML files in `.github/workflows`.
The current active workflows are:

### Linting - `lint.yml`

This workflow will run if:

* there is a pull request
* commits are pushed to these branches: `master`, `stable` and `rc/**`

The workflow has one job (`style`). It runs on `ubuntu` only and applies
yamllint and Hlint.

### Test suite - `unit-tests.yml`

This workflow will run if:

* there is a pull request
* commits are pushed to these branches: `master`, `stable` and `rc/**`.
* requested

The workflow has two jobs: `pedantic` and `unit-tests`.

The `pedantic` job runs on `ubuntu` only and builds Stack with the
`--pedantic` flag.

The `unit-tests` job runs on a matrix of operating systems and Stack
project-level configuration files (`stack.yaml`, by default). It builds and
tests Stack with the following flags: `--haddock --no-haddock-deps`.

Its approach to creating a cache depends on the operating system. Its 'Cache
dependencies on Unix-like OS' step caches the Stack root on Unix-like operating
systems. Its 'Cache dependencies on Windows' step caches the same information
on Windows, but takes into account that a relevant directory is located outside
of the Stack root.

### Integration-based - `integration-tests.yml`

This workflow will run if:

* there is a pull request
* commits are pushed to these branches: `master`, `stable` and `rc/**`
* any tag is created
* requested

The workflow has three jobs: `integration-tests`, `linux-arm64` and
`github-release`.

The `integration-tests` job runs on a matrix of operating systems (`ubuntu`,
`windows` and `macos`) and makes use of the `release.hs` script at
`etc/scripts`. Its approach to creating a cache is the same as for
`unit-tests.yml`, described above.

Its 'Install deps and run checks' step uses `release.hs check`.

Its 'Build bindist' step uses `release.hs build`.

Its 'Upload bindist' step uploads artifacts using the name of the runner's
operating system (`Linux`, `Windows` or `macOS`) as the name for the artifacts.

The `linux-arm64` job runs on a self-hosted runner for Linux and ARM64. It makes
use of Docker and a Docker file at `etc/dockerfiles/arm64.Dockerfile`.

Its 'Build bindist' step makes use of a compiled version of `release.hs` script
at `etc/scripts` to command `release build`.

Its 'Upload bindist' step uploads artifacts using `Linux-ARM64` as the name for
the artifacts.

The `github-release` job needs `integration-tests` and `linux-arm64`. It only
takes effect if the trigger for the workflow was the creation of a tag.

Its four steps `Download Linux/Windows/macOS/Linux-ARM64 artifact` download the
named artifacts to path `_release`.

Its step 'Hash and sign assets' makes use of a 'secret' environment variable
`RELEASE_SIGNING_KEY` established by the owner of the Stack repository. The
variable contains the private key for the GPG key with ID 0x575159689BEFB442.
That key is imported into GPG and then used by GPG to create a detached signature
for each file.

### Stan tool - `stan.yml`

[Stan](https://hackage.haskell.org/package/stan) is a Haskell static analysis
tool. As of `stan-0.1.0.1`, it supports GHC >= 9.6.3 and Stack is built with
GHC 9.6.6. The tool is configured by the contents of the `.stan.toml` file.

This workflow will run if:

* there is a pull request
* requested

## Haskell Language Server

You may be using [Visual Studio Code](https://code.visualstudio.com/) (VS Code)
with its
[Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell),
which is powered by the
[Haskell Language Server](https://github.com/haskell/haskell-language-server)
(HLS).

Stack can be built with Stack (which is recommended) or with Cabal (the tool).

=== "Stack"

    If you use Stack to build Stack, command `stack ghci` in the root directory
    of the Stack project should work as expected, if you have first commanded
    `stack build` once. `stack build` causes Cabal (the library) to create the
    automatically generated module `Stack_build`.

    If `ghc` is not on your PATH, then Haskell Language Server may report the
    following error about `Stack.Constants.ghcShowOptionsOutput`:
    ~~~text
    • Exception when trying to run compile-time code:
        ghc: readCreateProcess: does not exist (No such file or directory)
      Code: (TH.runIO (readProcess "ghc" ["--show-options"] "")
               >>= TH.lift . lines)
    • In the untyped splice:
        $(TH.runIO (readProcess "ghc" ["--show-options"] "") >>= TH.lift
            . lines)
    ~~~

    `ghc` should be on the PATH if you run VS Code itself in the Stack
    environment:
    ~~~text
    stack exec -- code .
    ~~~

    The following [cradle (`hie.yaml`)](https://github.com/haskell/hie-bios)
    should suffice to configure Haskell Language Server (HLS) explicitly for
    `./Setup.hs` and each of the buildable components in Stack's Cabal file:
    ~~~yaml
    cradle:
      multi:
      - path: "./Setup.hs"
        config:
          cradle:
            direct:
              arguments: []
      - path: "./"
        config:
          cradle:
            stack:
            - path: "./src"
              component: "stack:lib"
            - path: "./app"
              component: "stack:exe:stack"
            - path: "./tests/integration"
              component: "stack:exe:stack-integration-test"
            - path: "./tests/unit"
              component: "stack:test:stack-unit-test"
    ~~~

=== "Cabal (the tool)"

    If you use Cabal (the tool) to build Stack, command `cabal repl` in the root
    directory of the Stack project should work as expected, if you have GHC and
    (on Windows) MSYS2 on the PATH. Stack's custom `./Setup.hs` causes
    `cabal repl` to cause Cabal (the library) to create the automatically
    generated module `Stack_build`.

    If `ghc` is not on your PATH, then Haskell Language Server may report the
    following error about `Stack.Constants.ghcShowOptionsOutput`:
    ~~~text
    • Exception when trying to run compile-time code:
        ghc: readCreateProcess: does not exist (No such file or directory)
      Code: (TH.runIO (readProcess "ghc" ["--show-options"] "")
               >>= TH.lift . lines)
    • In the untyped splice:
        $(TH.runIO (readProcess "ghc" ["--show-options"] "") >>= TH.lift
            . lines)
    ~~~

    `ghc` and (on Windows) MSYS2 should be on the PATH if you run commands
    (including `cabal`) in the Stack environment:
    ~~~text
    stack exec --no-ghc-package-path -- cabal repl
    ~~~

    or
    ~~~text
    stack exec --no-ghc-package-path -- code .
    ~~~

    Use of GHC's environment variable `GHC_PACKAGE_PATH` is not compatible with
    Cabal (the tool). That is why the `--no-ghc-package-path` flag must be
    specified with `stack exec` when relying on Cabal (the tool).

    The following [cradle (`hie.yaml`)](https://github.com/haskell/hie-bios)
    should suffice to configure Haskell Language Server (HLS) explicitly for
    `./Setup.hs` and each of the buildable components in Stack's Cabal file:
    ~~~yaml
    cradle:
      multi:
      - path: "./Setup.hs"
        config:
          cradle:
            direct:
              arguments: []
      - path: "./"
        config:
          cradle:
            cabal:
            - path: "./src"
              component: "lib:stack"
            - path: "./app"
              component: "exe:stack"
            - path: "./tests/integration"
              component: "exe:stack-integration-test"
            - path: "./tests/unit"
              component: "test:stack-unit-test"
    ~~~

A cradle is not committed to Stack's repository because it imposes a choice of
tool used for building.

## Dev Containers

A [Development Container](https://containers.dev) (or Dev Container for short)
allows you to use a container as a full‑featured development environment.

You can run Dev Containers locally/remotely (with VS Code), or create a
[Codespace](https://github.com/features/codespaces) for a branch in a
repository to develop online.

Stack's default Dev Container is intended for use with its default
project‑level configuration (`stack.yaml`). But there are also Dev Containers
for the experimental project‑level configurations.

For further information, see the documentation for
[Dev Containers](dev_containers.md).

## Slack channel

If you're making deep changes and real-time communication with the Stack team
would be helpful, we have a `#stack-collaborators` Slack channel in the
Haskell Foundation workspace. To join the workspace, follow this
[link](https://haskell-foundation.slack.com/join/shared_invite/zt-z45o9x38-8L55P27r12YO0YeEufcO2w#/shared-invite/email).

## Matrix room

There is also a
[Haskell Stack room](https://matrix.to/#/#haskell-stack:matrix.org)
at address `#haskell-stack:matrix.org` on [Matrix](https://matrix.org/).
