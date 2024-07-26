<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 2. Building your project

The [`stack build`](../commands/build_command.md) command is the heart of Stack.
It is the engine that powers building your code, testing it, getting
dependencies, and more. Much of the remainder of this getting started guide will
cover its features.

!!! note

    Using the `build` command twice with the same options and arguments should
    generally do nothing (besides things like rerunning test suites), and
    should, in general, produce a reproducible result between different runs.

## Adding dependencies

A Haskell package often depends on code exposed by other Haskell packages.

Let's say we decide to modify our existing `helloworld` package source code to
use a new library, the one provided by the
[`text`](https://hackage.haskell.org/package/text) package.

We can modify `src/Lib.hs` so that its contents are as follows (click
:material-plus-circle: to learn more):

~~~haskell
{-# LANGUAGE OverloadedStrings #-} -- (1)!

module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T -- (2)!

someFunc :: IO ()
someFunc = T.putStrLn "someFunc" --(3)!
~~~

1.  Enables overloaded string literals. String literals now have type
    `(IsString a) => a`.

2.  The module is exposed by the library of the `text` package.

3.  `Data.Text.IO.putStrLn :: Text -> IO ()`.

If we command:

~~~text
stack build
~~~

Stack will report error [S-7282] during the build:

~~~text
...
Building library for helloworld-0.1.0.0..
[1 of 2] Compiling Lib [Source file changed]

src\Lib.hs:7:1: error:
    Could not load module ‘Data.Text.IO’
    It is a member of the hidden package ‘text-2.0.2’.
    Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
7 | import qualified Data.Text.IO as T
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Error: [S-7282]
       Stack failed to execute the build plan.

       While executing the build plan, Stack encountered the error:

       [S-7011]
       While building package helloworld-0.1.0.0 (scroll up to its section to see the error) using:
       ...
       Process exited with code: ExitFailure 1
~~~

The error `Could not load module ...` means that the package exposing the module
in question is not available.

To tell Stack that the `text` package is a dependency of the `helloworld`
package, you need to update the package description file (`package.yaml`).
Specifically, you need to add `text` under the `dependencies` key, like this:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text # added
~~~

Now, if we command again:

~~~text
stack build
~~~

we should get a successful result.

The output means that the `text` package was downloaded, configured, built, and
locally installed. Once that was done, we moved on to building our project
package (`helloworld`). At no point did we need to ask Stack to build
dependencies — it does so automatically.

## Listing dependencies

Let's have Stack add a few more dependencies to our project. First, we'll
include two new packages in the `dependencies` section for our library in our
`package.yaml`:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text
- filepath # added
- containers # added
~~~

After adding these two dependencies, we can again command:

~~~text
stack build
~~~

to have them downloaded, configured, built, and locally installed.

To find out which versions of these packages Stack installed, we can command:

~~~text
stack ls dependencies
~~~

## Packages not in the snapshot

The packages `text`, `filepath` and `containers` have something in common: they
are all provided with GHC (referred to as GHC boot packages).

Let's try a dependency on a more off-the-beaten-track package: the joke
[acme-missiles](http://www.stackage.org/package/acme-missiles) package.

We can further modify `src/Lib.hs` so that its contents are as follows:

~~~haskell
module Lib
    ( someFunc
    ) where

import Acme.Missiles ( launchMissiles )

someFunc :: IO ()
someFunc = launchMissiles
~~~

As before, to tell Stack that the `acme-missiles` package is a dependency of the
`helloworld` package, we can update the package description file
(`package.yaml`). The relevant part of the file now looks like this:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text
- filepath
- containers
- acme-missiles # added
~~~

If we command:

~~~text
stack build
~~~

Stack will report error [S-4804]:

~~~text
Error: [S-4804]
       Stack failed to construct a build plan.

       While constructing the build plan, Stack encountered the following errors. The
       'Stack configuration' refers to the set of package versions specified by the
       snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
       package is replaced, Stack prunes all other such packages that depend on it) and
       any extra-deps:

       In the dependencies for helloworld-0.1.0.0:
         * acme-missiles needed, but no version is in the Stack configuration (latest
           matching version is 0.3).
       The above is/are needed since helloworld is a build target.

       Some different approaches to resolving some or all of this:

         * Recommended action: try adding the following to your extra-deps in
           ...\helloworld\stack.yaml (project-level configuration):

           - acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1,613
~~~

It says that Stack was unable to construct the build plan.

This brings us to the next major topic in using Stack.

## Extending snapshots

A snapshot specifies a version of GHC and a set of package versions.

Remember above when `stack new` selected some
[LTS snapshot](https://github.com/commercialhaskell/lts-haskell#readme) for us?
That defined our build plan and available packages. When we tried using the
`text` package, it just worked, because it was part of the LTS *package set*.

We have updated the description of the `helloworld` package (in `package.yaml`)
to specify that it depends on the `acme-missiles` package, but `acme-missiles`
is not a member of the set of packages specified by the snapshot. So building
failed.

To add a version of `acme-missiles` to the available package versions, we'll use
the `extra-deps` key in Stack's project-level configuration file (`stack.yaml`).
That key defines extra package versions, not present in the snapshot, that will
be needed as dependencies. You can add this like so:

~~~yaml
extra-deps:
- acme-missiles-0.3 # not in the LTS snapshot
~~~

Now, if we command again:

~~~text
stack build
~~~

we should get a successful result.

With that out of the way, let's dig a little bit more into these snapshots. We
mentioned the LTS snapshots, and you can get information about it at
[https://www.stackage.org/lts](https://www.stackage.org/lts), including:

* The appropriate value (`lts-22.30`, as is currently the latest LTS)
* The GHC version used
* A full list of all packages versions available in this snapshot
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

## Available snapshots

Let's explore package sets a bit further. Instead of `lts-22.13`, let's change
our `stack.yaml` file to use the
[latest nightly](https://www.stackage.org/nightly). Right now, this is currently
2024-03-20 - please see the snapshot from the link above to get the latest.

Then, commanding `stack build` again will produce:

~~~text
stack build
# Downloaded nightly-2024-03-20 build plan.
# build output ...
~~~

We can also change snapshots on the command line, which can be useful in a
Continuous Integration (CI) setting, like on Travis. For example, command:

~~~text
stack --snapshot lts-21.25 build
# Downloaded lts-21.25 build plan.
# build output ...
~~~

When passed on the command line, you also get some additional "short-cut"
versions of snapshots: `--snapshot nightly` will use the newest Nightly snapshot
available, `--snapshot lts` will use the newest LTS, and `--snapshot lts-22`
will use the newest LTS in the 22.x series. The reason these are only available
on the command line and not in your `stack.yaml` file is that using them:

1. Will slow down your build (since Stack then needs to download information on
   the latest available LTS each time it builds)
2. Produces unreliable results (since a build run today may proceed differently
   tomorrow because of changes outside of your control)

We've mentioned `nightly-YYYY-MM-DD` and `lts-X.Y` values for the snapshot.
There are actually other options available, and the list will grow over time.
At the time of writing:

* `ghc-X.Y.Z`, for requiring a specific GHC version but no additional packages
* Experimental custom snapshot support

The most up-to-date information can always be found in the
[stack.yaml documentation](../configure/yaml/project.md#snapshot).

## Snapshots and GHC versions

As mentioned, a snapshot specifies a version of GHC as well as a set of package
versions.

Let's try using an older LTS snapshot. We'll use the newest 21.x snapshot with
the command:

~~~text
stack --snapshot lts-21 build
~~~

This succeeds, automatically installing the necessary GHC along the way. So, we
see that different LTS versions use different GHC versions and Stack can handle
that.

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
