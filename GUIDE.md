__NOTE__: This document is a work in progress, and is being shared to get
feedback. Please do *not* rely on this document as being perfect, and if you
see something that can be improved, either send a pull request or send a note
to Michael Snoyman (who's currently authoring it).

stack is a cross-platform program for developing Haskell projects. This guide
is intended to step a new stack user through all of the typical stack
workflows. This guide will not teach you Haskell, but will also not be looking
at much code. This guide will not presume prior experience with the Haskell
packaging system or other build tools.

## What is stack?

stack is a modern build tool for Haskell code. It handles the management of
your toolchain (including GHC- the Glasgow Haskell Compiler- and- for Windows
users- MSYS), building and registry libraries, building build tool
dependencies, and much more. While stack can use existing tools on your system,
stack has the capability to be your one-stop shop for all Haskell tooling you
need. This guide will follows that approach.

What makes stack special? It's primary design point is __reproducible builds__.
The goal is that if you run `stack build` today, you'll get the same result
running `stack build` tomorrow. There are some exceptions to that rule (changes
in your operating system configuration, for example), but overall it follows
this design philosophy closely.

stack has also been designed from the ground up to be user friendly, with an
intuitive, discoverable command line interface. For many users, simply
downloading stack and reading `stack --help` will be enough to get up and
running. This guide is intended to provide a gradual learning process for users
who prefer that learning style.

Finally, stack is __isolated__: it will not make changes outside of specific
stack directories (described below). Do not be worried if you see comments like
"Installing GHC": stack will not tamper with your system packages at all.
Additionally, stack packages will not interfere with packages installed by
other build tools like cabal.

_NOTE_ In this guide, I'll be running commands on a Linux system (Ubuntu 14.04,
64-bit) and sharing output from there. Output on other systems- or with
different versions of stack- will be slightly different. But all commands work
in a cross platform way, unless explicitly stated otherwise.

## Downloading

There's [a wiki page dedicated to downloading
stack](https://github.com/commercialhaskell/stack/wiki/Downloads) which has the
most up-to-date information for a variety of operating systems, including
multiple Linux flavors. Instead of repeating that content here, please go check
out that page and come back here when you can successfully run `stack
--version`. The rest of this session will demonstrate the installation
procedure on a vanilla Ubuntu 14.04 machine.

```
# Starting with a *really* bare machine
michael@d30748af6d3d:~$ sudo apt-get install wget
# Demonstrate that stack really isn't available
michael@d30748af6d3d:~$ stack
-bash: stack: command not found
# Get the signing key for the package repo
michael@d30748af6d3d:~$ wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
OK
michael@d30748af6d3d:~$ echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
deb http://download.fpcomplete.com/ubuntu/trusty stable main
michael@d30748af6d3d:~$ sudo apt-get update && sudo apt-get install stack -y
# downloading...
michael@d30748af6d3d:~$ stack --version
Version 0.1.3.1, Git revision 908b04205e6f436d4a5f420b1c6c646ed2b804d7
```

That's it, stack is now up and running, and you're good to go. In addition,
it's a good idea- though not required- to set your PATH environment variable to
include `$HOME/.local/bin`:

```
michael@d30748af6d3d:~$ echo 'export HOME=$HOME/.local/bin:$PATH' >> ~/.bashrc
```

## Hello World

Now that we've got stack, it's time to put it to work. We'll start off with the
`stack new` command to create a new *project*. We'll call our project
`helloworld`, and we'll use the `new-template` project template:

```
michael@d30748af6d3d:~$ stack new helloworld new-template
```

You'll see a lot of output since this is your first stack command, and there's
quite a bit of initial setup it needs to do, such as downloading the list of
packages available upstream. Here's an example of what you may see, though your
exact results may vary. Over the course of this guide a lot of the content will
begin to make more sense:

```
Downloading template "new-template" to create project "helloworld" in helloworld/ ...
Using the following authorship configuration:
author-email: example@example.com
author-name: Example Author Name
Copy these to /home/michael/.stack/stack.yaml and edit to use different values.
Writing default config file to: /home/michael/helloworld/stack.yaml
Basing on cabal files:
- /home/michael/helloworld/helloworld.cabal

Downloaded lts-3.2 build plan.
Caching build plan
Fetched package index.
Populated index cache.
Checking against build plan lts-3.2
Selected resolver: lts-3.2
Wrote project config to: /home/michael/helloworld/stack.yaml
```

Great, we now have a project in the `helloworld` directory. Let's go in there
and have some fun, using the most important stack command: `build`.

```
michael@d30748af6d3d:~$ cd helloworld/
michael@d30748af6d3d:~/helloworld$ stack build
No GHC found, expected version 7.10.2 (x86_64) (based on resolver setting in /home/michael/helloworld/stack.yaml).
Try running stack setup
```

That was a bit anticlimactic. The problem is that stack needs GHC in order to
build your project, but we don't have one on our system yet. Instead of
automatically assuming you want it to download and install GHC for you, stack
asks you to do this as a separate command: `setup`. Our message here lets us
know that `stack setup` will need to install GHC version 7.10.2. Let's try that
out:

```
michael@d30748af6d3d:~/helloworld$ stack setup
Downloaded ghc-7.10.2.
Installed GHC.
stack will use a locally installed GHC
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
```

It doesn't come through in the output here, but you'll get intermediate
download percentage statistics while the download is occurring. This command
may take some time, depending on download speeds.

__NOTE__: GHC gets installed to a stack-specific directory, so calling `ghc` on the
command line won't work. See the `stack exec`, `stack ghc`, and `stack runghc`
commands below for more information.

But now that we've got GHC available, stack can build our project:

```
michael@d30748af6d3d:~/helloworld$ stack build
helloworld-0.1.0.0: configure
Configuring helloworld-0.1.0.0...
helloworld-0.1.0.0: build
Preprocessing library helloworld-0.1.0.0...
[1 of 1] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/Lib.o )
In-place registering helloworld-0.1.0.0...
Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-exe/helloworld-exe-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-exe/helloworld-exe ...
helloworld-0.1.0.0: install
Installing library in
/home/michael/helloworld/.stack-work/install/x86_64-linux/lts-3.2/7.10.2/lib/x86_64-linux-ghc-7.10.2/helloworld-0.1.0.0-6urpPe0MO7OHasGCFSyIAT
Installing executable(s) in
/home/michael/helloworld/.stack-work/install/x86_64-linux/lts-3.2/7.10.2/bin
Registering helloworld-0.1.0.0...
```

If you look closely at the output, you can see that it built both a library
called "helloworld" and an executable called "helloworld-exe". We'll explain in
the next section where this information is defined. For now, though, let's just
run our executable (which just outputs the string "someFunc"):

```
michael@d30748af6d3d:~/helloworld$ stack exec helloworld-exe
someFunc
```

And finally, like all good software, helloworld actually has a test suite.
Let's run it with `stack test`:

```
michael@d30748af6d3d:~/helloworld$ stack test
NOTE: the test command is functionally equivalent to 'build --test'
helloworld-0.1.0.0: configure (test)
Configuring helloworld-0.1.0.0...
helloworld-0.1.0.0: build (test)
Preprocessing library helloworld-0.1.0.0...
In-place registering helloworld-0.1.0.0...
Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0...
[1 of 1] Compiling Main             ( test/Spec.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-test/helloworld-test-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-test/helloworld-test ...
helloworld-0.1.0.0: test (suite: helloworld-test)
Test suite not yet implemented
```

Reading the output, you'll see that stack first builds the test suite and then
automatically runs it for us. For both the `build` and `test` command, already
built components are not built again. You can see this by running `stack build`
and `stack test` a second time:

```
michael@d30748af6d3d:~/helloworld$ stack build
michael@d30748af6d3d:~/helloworld$ stack test
NOTE: the test command is functionally equivalent to 'build --test'
helloworld-0.1.0.0: test (suite: helloworld-test)
Test suite not yet implemented
```

In the next three subsections, we'll dissect a few details of this helloworld
example.

### Files in helloworld

Before moving on with understanding stack a bit better, let's understand our project just a bit better.

```
michael@d30748af6d3d:~/helloworld$ find * -type f
LICENSE
Setup.hs
app/Main.hs
helloworld.cabal
src/Lib.hs
stack.yaml
test/Spec.hs
```

The `app/Main.hs`, `src/Lib.hs`, and `test/Spec.hs` files are all Haskell
source files that compose the actual functionality of our project, and we won't
dwell on them too much. Similarly, the LICENSE file has no impact on the build,
but is there for informational/legal purposes only. That leaves Setup.hs,
helloworld.cabal, and stack.yaml.

The Setup.hs file is a component of the Cabal build system which stack uses.
It's technically not needed by stack, but it is still considered good practice
in the Haskell world to include it. The file we're using is straight
boilerplate:

```haskell
import Distribution.Simple
main = defaultMain
```

Next, let's look at our stack.yaml file, which gives our project-level settings:

```yaml
flags: {}
packages:
- '.'
extra-deps: []
resolver: lts-3.2
```

If you're familiar with YAML, you'll see that the flags and extra-deps keys
have empty values. We'll see more interesting usages for these fields later.
Let's focus on the other two fields. packages tells stack which local packages
to build. In our simple example, we have just a single package in our project,
located in the same directory, so `'.'` suffices. However, stack has powerful
support for multi-package projects, which we'll elaborate on as this guide
progresses.

The final field is resolver. This tells stack *how* to build your package:
which GHC version to use, versions of package dependencies, and so on. Our
value here says to use [LTS Haskell version
3.2](https://www.stackage.org/lts-3.2), which implies GHC 7.10.2 (which is why
`stack setup` installs that version of GHC). There are a number of values you
can use for resolver, which we'll talk about below.

The final file of import is helloworld.cabal. stack is built on top of the
Cabal build system. In Cabal, we have individual *packages*, each of which
contains a single .cabal file. The .cabal file can define 1 or more
*components*: a library, executables, test suites, and benchmarks. It also
specifies additional information such as library dependencies, default language
pragmas, and so on.

In this guide, we'll discuss the bare minimum necessary to understand how to
modify a .cabal file. The definitive reference on the .cabal file format is
[available on
haskell.org](https://www.haskell.org/cabal/users-guide/developing-packages.html).

### The setup command

As we saw above, the `setup` command installed GHC for us. Just for kicks,
let's run `setup` a second time:

```
michael@d30748af6d3d:~/helloworld$ stack setup
stack will use a locally installed GHC
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
```

Thankfully, the command is smart enough to know not to perform an installation
twice. setup will take advantage of either the first GHC it finds on your PATH,
or a locally installed version. As the command output above indicates, you can
use `stack path` for quite a bit of path information (which we'll play with
more later). For now, we'll just look at where GHC is installed:

```
michael@d30748af6d3d:~/helloworld$ stack exec which ghc
/home/michael/.stack/programs/x86_64-linux/ghc-7.10.2/bin/ghc
```

As you can see from that path, the installation is placed such that it will not
interfere with any other GHC installation, either system-wide, or even
different GHC versions installed by stack.

### The build command

The build command is the heart and soul of stack. It is the engine that powers
building your code, testing it, getting dependencies, and more. Quite a bit of
the remainder of this guide will cover fun things you can do with build to get
more advanced behavior, such as building test and Haddocks at the same time, or
constantly rebuilding blocking on file changes.

But on a philosophical note: running the build command twice with the same
options and arguments should generally be a no-op (besides things like
rerunning test suites), and should in general produce a reproducible result
between different runs.

OK, enough talking about this simple example. Let's start making it a bit more
complicated!

## Adding dependencies

Let's say we decide to modify our helloworld source a bit to use a new library,
perhaps the ubiquitous text package. For example:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"
```

When we try to build this, things don't go as expected:

```haskell
michael@d30748af6d3d:~/helloworld$ stack build
helloworld-0.1.0.0-c91e853ce4bfbf6d394f54b135573db8: unregistering (local file changes)
helloworld-0.1.0.0: configure
Configuring helloworld-0.1.0.0...
helloworld-0.1.0.0: build
Preprocessing library helloworld-0.1.0.0...

/home/michael/helloworld/src/Lib.hs:6:18:
    Could not find module `Data.Text.IO'
    Use -v to see a list of the files searched for.

--  While building package helloworld-0.1.0.0 using:
      /home/michael/.stack/programs/x86_64-linux/ghc-7.10.2/bin/runhaskell -package=Cabal-1.22.4.0 -clear-package-db -global-package-db -package-db=/home/michael/.stack/snapshots/x86_64-linux/lts-3.2/7.10.2/pkgdb/ /tmp/stack5846/Setup.hs --builddir=.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/ build exe:helloworld-exe --ghc-options -hpcdir .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/hpc/.hpc/ -ddump-hi -ddump-to-file
    Process exited with code: ExitFailure 1
```

Notice that it says "Could not find module." This means that the package
containing the module in question is not available. In order to tell stack that
you want to use text, you need to add it to your .cabal file. This can be done
in your build-depends section, and looks like this:

```cabal
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                       -- This next line is the new one
                     , text
  default-language:    Haskell2010
```

Now if we rerun `stack build`, we get a very different result:

```
michael@d30748af6d3d:~/helloworld$ stack build
text-1.2.1.3: download
text-1.2.1.3: configure
text-1.2.1.3: build
text-1.2.1.3: install
helloworld-0.1.0.0: configure
Configuring helloworld-0.1.0.0...
helloworld-0.1.0.0: build
Preprocessing library helloworld-0.1.0.0...
[1 of 1] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/Lib.o )
In-place registering helloworld-0.1.0.0...
Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-exe/helloworld-exe-tmp/Main.o ) [Lib changed]
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-exe/helloworld-exe ...
helloworld-0.1.0.0: install
Installing library in
/home/michael/helloworld/.stack-work/install/x86_64-linux/lts-3.2/7.10.2/lib/x86_64-linux-ghc-7.10.2/helloworld-0.1.0.0-HI1deOtDlWiAIDtsSJiOtw
Installing executable(s) in
/home/michael/helloworld/.stack-work/install/x86_64-linux/lts-3.2/7.10.2/bin
Registering helloworld-0.1.0.0...
Completed all 2 actions.
```

### extra-deps

Let's try a more off-the-beaten-track package: the joke
[acme-missiles](http://www.stackage.org/package/acme-missiles) package. Our
source code is simple:

```haskell
module Lib
    ( someFunc
    ) where

import Acme.Missiles

someFunc :: IO ()
someFunc = launchMissiles
```

As expected, `stack build` will fail because the module is not available. But
if we add acme-missiles to the .cabal file, we get a new error message:

```
michael@d30748af6d3d:~/helloworld$ stack build
While constructing the BuildPlan the following exceptions were encountered:

--  While attempting to add dependency,
    Could not find package acme-missiles in known packages

--  Failure when adding dependencies:
      acme-missiles: needed (-any), latest is 0.3, but not present in build plan
    needed for package: helloworld-0.1.0.0

Recommended action: try adding the following to your extra-deps in /home/michael/helloworld/stack.yaml
- acme-missiles-0.3

You may also want to try the 'stack solver' command
```

Notice that it says acme-missiles is "not present in build plan." This is the
next major topic to understand when using stack.

## Curated package sets

Remember up above when `stack new` selected the lts-3.2 resolver for us? That's
what's defining our build plan, and available packages. When we tried using the
text package, it just worked, because it was part of the lts-3.2 *package set*.
acme-missiles, on the other hand, is not part of that package set, and
therefore building failed.

The first thing you're probably wondering is: how do I fix this? To do so,
we'll use another one of the fields in stack.yaml- `extra-deps`- which is used
to define extra dependencies not present in your resolver. With that change,
our stack.yaml looks like:

```yaml
flags: {}
packages:
- '.'
extra-deps:
- acme-missiles-0.3 # Here it is
resolver: lts-3.2
```

And as expected, `stack build` succeeds.

With that out of the way, let's dig a little bit more into these package sets, also known as *snapshots*. We mentioned lts-3.2, and you can get quite a bit of information about it at [https://www.stackage.org/lts-3.2](https://www.stackage.org/lts-3.2):

* The appropriate resolver value (`resolver: lts-3.2`, as we used above)
* The GHC version used
* A full list of all packages available in this snapshot
* The ability to perform a Hoogle search on the packages in this snapshot
* A [list of all modules](https://www.stackage.org/lts-3.2/docs) in a snapshot, which an be useful when trying to determine which package to add to your .cabal file

You can also see a [list of all available
snapshots](https://www.stackage.org/snapshots). You'll notice two flavors: LTS
(standing for Long Term Support) and Nightly. You can read more about them on
the [LTS Haskell Github page](https://github.com/fpco/lts-haskell#readme). If
you're not sure what to go with, start with LTS Haskell. That's what stack will
lean towards by default as well.

## Resolvers and changing your compiler version

Now that we know a bit more about package sets, let's try putting that
knowledge to work. Instead of lts-3.2, let's change our stack.yaml file to use
[nightly-2015-08-26](https://www.stackage.org/nightly-2015-08-26). Rerunning
`stack build` will produce:

```
michael@d30748af6d3d:~/helloworld$ stack build
Downloaded nightly-2015-08-26 build plan.
Caching build plan
stm-2.4.4: configure
stm-2.4.4: build
stm-2.4.4: install
acme-missiles-0.3: configure
acme-missiles-0.3: build
acme-missiles-0.3: install
helloworld-0.1.0.0: configure
Configuring helloworld-0.1.0.0...
helloworld-0.1.0.0: build
Preprocessing library helloworld-0.1.0.0...
In-place registering helloworld-0.1.0.0...
Preprocessing executable 'helloworld-exe' for helloworld-0.1.0.0...
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-exe/helloworld-exe ...
helloworld-0.1.0.0: install
Installing library in
/home/michael/helloworld/.stack-work/install/x86_64-linux/nightly-2015-08-26/7.10.2/lib/x86_64-linux-ghc-7.10.2/helloworld-0.1.0.0-6cKaFKQBPsi7wB4XdqRv8w
Installing executable(s) in
/home/michael/helloworld/.stack-work/install/x86_64-linux/nightly-2015-08-26/7.10.2/bin
Registering helloworld-0.1.0.0...
Completed all 3 actions.
```

We can also change resolvers on the command line, which can be useful in a
Continuous Integration (CI) setting, like on Travis. For example:

```
michael@d30748af6d3d:~/helloworld$ stack --resolver lts-3.1 build
Downloaded lts-3.1 build plan.
Caching build plan
stm-2.4.4: configure
# Rest is the same, no point copying it
```

When passed on the command line, you also get some additional "short-cut" versions of resolvers: `--resolver nightly` will use the newest Nightly resolver available, `--resolver lts` will use the newest LTS, and `--resolver lts-2` will use the newest LTS in the 2.X series. The reason these are only available on the command line and not in your stack.yaml file is that using them:

1. Will slow your build down, since stack needs to download information on the
   latest available LTS each time it builds
2. Produces unreliable results, since a build run today may proceed differently
   tomorrow because of changes outside of your control.

### Changing GHC versions

Finally, let's try using an older LTS snapshot. We'll use the newest 2.X
snapshot:

```
michael@d30748af6d3d:~/helloworld$ stack --resolver lts-2 build
Selected resolver: lts-2.22
Downloaded lts-2.22 build plan.
Caching build plan
No GHC found, expected version 7.8.4 (x86_64) (based on resolver setting in /home/michael/helloworld/stack.yaml). Try running stack setup
```

This fails, because GHC 7.8.4 (which lts-2.22 uses) is not available on our
system. The first lesson is: when you want to change your GHC version, modify
the resolver value. Now the question is: how do we get the right GHC version?
One answer is to use `stack setup` like we did above, this time with the
`--resolver lts-2` option. However, there's another way worth mentioning: the
`--install-ghc` flag.

```
michael@d30748af6d3d:~/helloworld$ stack --resolver lts-2 --install-ghc build
Selected resolver: lts-2.22
Downloaded ghc-7.8.4.
Installed GHC.
stm-2.4.4: configure
# Mostly same as before, nothing interesting to see
```

What's nice about `--install-ghc` is that:

1. You don't need to have an extra step in your build script
2. It only requires downloading the information on latest snapshots once

As mentioned above, the default behavior of stack is to *not* install new
versions of GHC automatically, to avoid surprising users with large
downloads/installs. This flag simply changes that default behavior.

### Other resolver values

We've mentioned `nightly-YYYY-MM-DD` and `lts-X.Y` values for the resolver.
There are actually other options available, and the list will grow over time.
At the time of writing:

* `ghc-X.Y.Z`, for requiring a specific GHC version but no additional packages
* Experimental GHCJS support
* Experimental custom snapshot support

The most up-to-date information can always be found on the [stack.yaml wiki
page](https://github.com/commercialhaskell/stack/wiki/stack.yaml#resolver).

## Existing projects

Alright, enough playing around with simple projects. Let's take an open source
package and try to build it. We'll be ambitious and use
[yackage](https://www.stackage.org/package/yackage), a local package server
using [Yesod](http://www.yesodweb.com/). To get the code, we'll use the `stack
unpack` command:

```
michael@d30748af6d3d:~$ stack unpack yackage-0.8.0
yackage-0.8.0: download
Unpacked yackage-0.8.0 to /home/michael/yackage-0.8.0/
michael@d30748af6d3d:~$ cd yackage-0.8.0/
```

This new directory does not have a stack.yaml file, so we need to make one
first. We could do it by hand, but let's be lazy instead with the `stack init`
command:

```
michael@d30748af6d3d:~/yackage-0.8.0$ stack init
Writing default config file to: /home/michael/yackage-0.8.0/stack.yaml
Basing on cabal files:
- /home/michael/yackage-0.8.0/yackage.cabal

Checking against build plan lts-3.2
Selected resolver: lts-3.2
Wrote project config to: /home/michael/yackage-0.8.0/stack.yaml
michael@d30748af6d3d:~/yackage-0.8.0$ cat stack.yaml
flags:
  yackage:
    upload: true
packages:
- '.'
extra-deps: []
resolver: lts-3.2
```

stack init does quite a few things for you behind the scenes:

* Creates a list of snapshots that would be good candidates. The basic algorithm here is: prefer snapshots you've already built some packages for (to increase sharing of binary package databases, as we'll discuss later), prefer recent snapshots, and prefer LTS. These preferences can be tweaked with command line flags, see `stack init --help`.
* Finds all of the .cabal files in your current directory and subdirectories (unless you use `--ignore-subdirs`) and determines the packages and versions they require
* Finds a combination of snapshot and package flags that allows everything to compile

Assuming it finds a match, it will write your stack.yaml file, and everything
will be good. Given that LTS Haskell and Stackage Nightly have ~1400 of the
most common Haskell packages, this will often be enough. However, let's
simulate a failure by adding acme-missiles to our build-depends and re-initing:

```
michael@d30748af6d3d:~/yackage-0.8.0$ stack init --force
Writing default config file to: /home/michael/yackage-0.8.0/stack.yaml
Basing on cabal files:
- /home/michael/yackage-0.8.0/yackage.cabal

Checking against build plan lts-3.2

* Build plan did not match your requirements:
    acme-missiles not found
    - yackage requires -any

Checking against build plan lts-3.1

* Build plan did not match your requirements:
    acme-missiles not found
    - yackage requires -any


Checking against build plan nightly-2015-08-26

* Build plan did not match your requirements:
    acme-missiles not found
    - yackage requires -any


Checking against build plan lts-2.22

* Build plan did not match your requirements:
    acme-missiles not found
    - yackage requires -any

    warp version 3.0.13.1 found
    - yackage requires >=3.1


There was no snapshot found that matched the package bounds in your .cabal files.
Please choose one of the following commands to get started.

    stack init --resolver lts-3.2
    stack init --resolver lts-3.1
    stack init --resolver nightly-2015-08-26
    stack init --resolver lts-2.22

You'll then need to add some extra-deps. See:

    https://github.com/commercialhaskell/stack/wiki/stack.yaml#extra-deps

You can also try falling back to a dependency solver with:

    stack init --solver
```

stack has tested four different snapshots, and in every case discovered that
acme-missiles is not available. Also, when testing lts-2.22, it found that the
warp version provided was too old for yackage. The question is: what do we do
next?

The recommended approach is: pick a resolver, and fix the problem. Again,
following the advice mentioned above, default to LTS if you don't have a
preference. In this case, the newest LTS listed is lts-3.2. Let's pick that.
stack has told us the correct command to do this. We'll just remove our old
stack.yaml first and then run it:

```
michael@d30748af6d3d:~/yackage-0.8.0$ rm stack.yaml
michael@d30748af6d3d:~/yackage-0.8.0$ stack init --resolver lts-3.2
Writing default config file to: /home/michael/yackage-0.8.0/stack.yaml
Basing on cabal files:
- /home/michael/yackage-0.8.0/yackage.cabal

Checking against build plan lts-3.2

* Build plan did not match your requirements:
    acme-missiles not found
    - yackage requires -any


Selected resolver: lts-3.2
Wrote project config to: /home/michael/yackage-0.8.0/stack.yaml
```

As you may guess, `stack build` will now fail due to the missing acme-missiles.
Toward the end of the error message, it says the familiar:

```
Recommended action: try adding the following to your extra-deps in /home/michael/yackage-0.8.0/stack.yaml
- acme-missiles-0.3
```

If you're following along at home, try making the necessary stack.yaml
modification to get things building.

### Alternative solution: dependency solving

There's another solution to the problem you may consider. At the very end of
the previous error message, it said:

```
You may also want to try the 'stack solver' command
```

This approach uses a full blown dependency solver to look at all upstream package versions available and compare them to your snapshot selection and version ranges in your .cabal file. In order to use this feature, you'll need the cabal executable available. Let's build that with:

```
michael@d30748af6d3d:~/yackage-0.8.0$ stack build cabal-install
random-1.1: download
mtl-2.2.1: download
network-2.6.2.1: download
old-locale-1.0.0.7: download
random-1.1: configure
random-1.1: build
# ...
cabal-install-1.22.6.0: download
cabal-install-1.22.6.0: configure
cabal-install-1.22.6.0: build
cabal-install-1.22.6.0: install
Completed all 10 actions.
```

Now we can use `stack solver`:

```
michael@d30748af6d3d:~/yackage-0.8.0$ stack solver
This command is not guaranteed to give you a perfect build plan
It's possible that even with the changes generated below, you will still need to do some manual tweaking
Asking cabal to calculate a build plan, please wait
extra-deps:
- acme-missiles-0.3
```

And if we're exceptionally lazy, we can ask stack to modify our stack.yaml file
for us:

```
michael@d30748af6d3d:~/yackage-0.8.0$ stack solver --modify-stack-yaml
This command is not guaranteed to give you a perfect build plan
It's possible that even with the changes generated below, you will still need to do some manual tweaking
Asking cabal to calculate a build plan, please wait
extra-deps:
- acme-missiles-0.3
Updated /home/michael/yackage-0.8.0/stack.yaml
```

With that change, `stack build` will now run.

NOTE: You should probably back up your stack.yaml before doing this, such as
committing to Git/Mercurial/Darcs.

There's one final approach to mention: skipping the snapshot entirely and just
using dependency solving. You can do this with the `--solver` flag to `init`.
This is not a commonly used workflow with stack, as you end up with a large
number of extra-deps, and no guarantee that the packages will compile together.
For those interested, however, the option is available. You need to make sure
you have both the ghc and cabal commands on your PATH. An easy way to do this
is to use the `stack exec` command:

```
michael@d30748af6d3d:~/yackage-0.8.0$ stack exec --no-ghc-package-path -- stack init --solver --force
Writing default config file to: /home/michael/yackage-0.8.0/stack.yaml
Basing on cabal files:
- /home/michael/yackage-0.8.0/yackage.cabal

Asking cabal to calculate a build plan, please wait
Selected resolver: ghc-7.10
Wrote project config to: /home/michael/yackage-0.8.0/stack.yaml
```

The --no-ghc-package-path flag is described below, and is only needed due to a
[bug](https://github.com/commercialhaskell/stack/issues/860) in the currently
released stack. That bug is fixed in 0.1.4 and forward.

## Different databases

## The build synonyms

* build, test, bench, haddock, and install
* Building executables (alex, happy, warp, etc)

## Flags and GHC options

## Targets, locals, and extra-deps

* https://github.com/commercialhaskell/stack/wiki/Build-command
* Upstream packages from Github and HTTPS

## Multi-package projects

* --stack-yaml command line flag
* `STACK_YAML`
* --resolver command line flag

## Multiple stack.yaml files

## path

* How to completely remove traces of stack

## exec

* --no-ghc-package-path

## repl

## ghc/runghc

## Implicit global

## IDE

## Templates

## stack.yaml details

* Link to [Wiki page](https://github.com/commercialhaskell/stack/wiki/stack.yaml)

## stack.yaml vs .cabal files

## Comparison to other tools

* Curation vs dependency solving as a default
* Automatically building dependencies
* Reproducible
* Isolated by default (no need for sandboxes!)

## More resources

* `stack --help`
* `stack --version`
* `--verbose` (or `-v`) flag
* Mailing list/Stack Overflow/Wiki

## Fun features

### stack dot

### Travis with caching

### Script interpreter

### Docker

* Don't forget creating images!

### Shell autocompletion

### Custom snapshots

## Power user commands

* update
* unpack
* sdist
* upload
* upgrade
* list-dependencies
