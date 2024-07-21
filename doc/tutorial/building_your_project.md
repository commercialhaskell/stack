  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 2. Building your project

The `build` command is the heart and soul of Stack. It is the engine that powers
building your code, testing it, getting dependencies, and more. Quite a bit of
the remainder of this guide will cover more advanced `build` functions and
features, such as building test and Haddocks at the same time, or constantly
rebuilding blocking on file changes.

!!! note

    Using the `build` command twice with the same options and arguments should
    generally do nothing (besides things like rerunning test suites), and
    should, in general, produce a reproducible result between different runs.

## Adding dependencies

Let's say we decide to modify our `helloworld` source a bit to use a new
library, perhaps the ubiquitous `text` package. In `src/Lib.hs`, we can, for
example add:

~~~haskell
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"
~~~

When we try to build this, things don't go as expected:

~~~text
stack build
# build failure output (abridged for clarity) ...
src\Lib.hs:6:1: error:
    Could not load module ‘Data.Text.IO’
    It is a member of the hidden package ‘text-1.2.5.0’.
    Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
6 | import qualified Data.Text.IO as T
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
~~~

This means that the package containing the module in question is not available.
To tell Stack to use [text](https://hackage.haskell.org/package/text), you need
to add it to your `package.yaml` file — specifically in your `dependencies`
section, like this:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text # added here
~~~

Now if we rerun `stack build`, we should get a successful result. Command:

~~~text
stack build
# build output ...
~~~

This output means that the `text` package was downloaded, configured, built, and
locally installed. Once that was done, we moved on to building our project
package (`helloworld`). At no point did we need to ask Stack to build
dependencies — it does so automatically.

### Listing Dependencies

Let's have Stack add a few more dependencies to our project. First, we'll
include two new packages in the `dependencies` section for our library in our
`package.yaml`:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text
- filepath
- containers
~~~

After adding these two dependencies, we can again run `stack build` to have them
installed. Command:

~~~text
stack build
# build output ...
~~~

Finally, to find out which versions of these libraries Stack installed, we can
ask Stack to `ls dependencies`. Command:

~~~text
stack ls dependencies
# dependency output ...
~~~

### extra-deps

Let's try a more off-the-beaten-track package: the joke
[acme-missiles](http://www.stackage.org/package/acme-missiles) package. Our
source code is simple:

~~~haskell
module Lib
    ( someFunc
    ) where

import Acme.Missiles

someFunc :: IO ()
someFunc = launchMissiles
~~~

Again, we add this new dependency to the `package.yaml` file like this:

~~~yaml
dependencies:
- base >= 4.7 && < 5
- text
- filepath
- containers
- acme-missiles # added
~~~

However, rerunning `stack build` shows us the following error message. Command:

~~~text
stack build
# build failure output ...
~~~

It says that it was unable to construct the build plan.

This brings us to the next major topic in using Stack.

## Curated package sets

Remember above when `stack new` selected some
[LTS snapshot](https://github.com/commercialhaskell/lts-haskell#readme) for us?
That defined our build plan and available packages. When we tried using the
`text` package, it just worked, because it was part of the LTS *package set*.

We've specified the `acme-missiles` package in the `package.yaml` file (see
above), but `acme-missiles` is not part of that LTS package set, so building
failed.

To add `acme-missiles` to the available packages, we'll use the `extra-deps` key
in the `stack.yaml` file. That key defines extra packages, not present in the
snapshot, that will be needed as dependencies. You can add this like so:

~~~yaml
extra-deps:
- acme-missiles-0.3 # not in the LTS snapshot
~~~

Now `stack build` will succeed.

With that out of the way, let's dig a little bit more into these package sets,
also known as *snapshots*. We mentioned the LTS snapshots, and you can get quite
a bit of information about it at
[https://www.stackage.org/lts](https://www.stackage.org/lts), including:

* The appropriate value (`lts-22.13`, as is currently the latest LTS)
* The GHC version used
* A full list of all packages available in this snapshot
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

## Snapshots and changing your compiler version

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

### Changing GHC versions

Finally, let's try using an older LTS snapshot. We'll use the newest 21.x
snapshot with the command:

~~~text
stack --snapshot lts-21 build
# build output ...
~~~

This succeeds, automatically installing the necessary GHC along the way. So, we
see that different LTS versions use different GHC versions and Stack can handle
that.

### Other snapshot values

We've mentioned `nightly-YYYY-MM-DD` and `lts-X.Y` values for the snapshot.
There are actually other options available, and the list will grow over time.
At the time of writing:

* `ghc-X.Y.Z`, for requiring a specific GHC version but no additional packages
* Experimental custom snapshot support

The most up-to-date information can always be found in the
[stack.yaml documentation](../configure/yaml/project.md#snapshot).

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
