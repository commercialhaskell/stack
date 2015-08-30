stack is a cross-platform program for developing Haskell projects. This guide
is intended to step a new stack user through all of the typical stack
workflows. This guide will not teach you Haskell, but will also not be looking
at much code. This guide will not presume prior experience with the Haskell
packaging system or other build tools.

## What is stack?

stack is a modern build tool for Haskell code. It handles the management of
your toolchain (including GHC- the Glasgow Haskell Compiler- and- for Windows
users- MSYS), building and registering libraries, building build tool
dependencies, and much more. While stack can use existing tools on your system,
stack has the capability to be your one-stop shop for all Haskell tooling you
need. This guide will follow that approach.

What makes stack special? Its primary design point is __reproducible builds__.
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
michael@d30748af6d3d:~$ echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc
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

What this output means is: the text package was downloaded, configured, built,
and locally installed. Once that was done, we moved on to building our local
package (helloworld). Notice that at no point do you need to ask stack to build
dependencies for you: it does so automatically.

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

Time to take a short break from hands-on examples and discuss a little
architecture. stack has the concept of multiple *databases*. A database
consists of a GHC package database (which contains the compiled version of a
library), executables, and a few other things as well. Just to give you an
idea:

```
michael@d30748af6d3d:~/helloworld$ ls .stack-work/install/x86_64-linux/lts-3.2/7.10.2/
bin  doc  flag-cache  lib  pkgdb
```

Databases in stack are *layered*. For example, the database listing I just gave
is what we call a *local* database. This is layered on top of a *snapshot*
database, which contains the libraries and executables specified in the
snapshot itself. Finally, GHC itself ships with a number of libraries and
executables, which forms the *global* database. Just to give a quick idea of
this, we can look at the output of the `ghc-pkg list` command in our helloworld
project:

```
/home/michael/.stack/programs/x86_64-linux/ghc-7.10.2/lib/ghc-7.10.2/package.conf.d
   Cabal-1.22.4.0
   array-0.5.1.0
   base-4.8.1.0
   bin-package-db-0.0.0.0
   binary-0.7.5.0
   bytestring-0.10.6.0
   containers-0.5.6.2
   deepseq-1.4.1.1
   directory-1.2.2.0
   filepath-1.4.0.0
   ghc-7.10.2
   ghc-prim-0.4.0.0
   haskeline-0.7.2.1
   hoopl-3.10.0.2
   hpc-0.6.0.2
   integer-gmp-1.0.0.0
   pretty-1.1.2.0
   process-1.2.3.0
   rts-1.0
   template-haskell-2.10.0.0
   terminfo-0.4.0.1
   time-1.5.0.1
   transformers-0.4.2.0
   unix-2.7.1.0
   xhtml-3000.2.1
/home/michael/.stack/snapshots/x86_64-linux/nightly-2015-08-26/7.10.2/pkgdb
   stm-2.4.4
/home/michael/helloworld/.stack-work/install/x86_64-linux/nightly-2015-08-26/7.10.2/pkgdb
   acme-missiles-0.3
   helloworld-0.1.0.0
```

Notice that acme-missiles ends up in the *local* database. Anything which is
not installed from a snapshot ends up in the local database. This includes:
your own code, extra-deps, and in some cases even snapshot packages, if you
modify them in some way. The reason we have this structure is that:

* it lets multiple projects reuse the same binary builds of many snapshot packages,
* but doesn't allow different projects to "contaminate" each other by putting non-standard content into the shared snapshot database

Typically, the process by which a snapshot package is marked as modified is
referred to as "promoting to an extra-dep," meaning we treat it just like a
package in the extra-deps section. This happens for a variety of reasons,
including:

* changing the version of the snapshot package
* changing build flags
* one of the packages that the package depends on has been promoted to an extra-dep

And as you probably guessed: there are multiple snapshot databases available, e.g.:

```
michael@d30748af6d3d:~/helloworld$ ls ~/.stack/snapshots/x86_64-linux/
lts-2.22  lts-3.1  lts-3.2  nightly-2015-08-26
```

These databases don't get layered on top of each other, but are each used separately.

In reality, you'll rarely- if ever- interact directly with these databases, but
it's good to have a basic understanding of how they work so you can understand
why rebuilding may occur at different points.

## The build synonyms

Let me show you a subset of the `stack --help` output:

```
build    Build the project(s) in this directory/configuration
install  Shortcut for 'build --copy-bins'
test     Shortcut for 'build --test'
bench    Shortcut for 'build --bench'
haddock  Shortcut for 'build --haddock'
```

It's important to note that four of these commands are just synonyms for the
`build` command. They are provided for convenience for common cases (e.g.,
`stack test` instead of `stack build --test`) and so that commonly expected
commands just work.

What's so special about these commands being synonyms? It allows us to make
much more composable command lines. For example, we can have a command that
builds executables, generates Haddock documentation (Haskell API-level docs),
and builds and runs your test suites, with:

```
stack build --haddock --test
```

You can even get more inventive as you learn about other flags. For example,
take the following:

```
stack build --pedantic --haddock --test --exec "echo Yay, it succeeded" --file-watch
```

This will:

* turn on all warnings and errors
* build your library and executables
* generate Haddocks
* build and run your test suite
* run the command `echo Yay, it succeeded` when that completes
* after building, watch for changes in the files used to build the project, and kick off a new build when done

### install and copy-bins

It's worth calling out the behavior of the install command and `--copy-bins`
option, since this has confused a number of users, especially when compared to
behavior of other tools (e.g., cabal-install). The `install` command does
precisely one thing in addition to the build command: it copies any generated
executables to the local bin path. You may recognize the default value for that
path:

```
michael@d30748af6d3d:~/helloworld$ stack path --local-bin-path
/home/michael/.local/bin
```

That's why the download page recommends adding that directory to your `PATH`
environment variable. This feature is convenient, because now you can simply
run `executable-name` in your shell instead of having to run `stack exec
executable-name` from inside your project directory.

Since it's such a point of confusion, let me list a number of things stack does
*not* do specially for the install command:

* stack will always build any necessary dependencies for your code. The install command is not necessary to trigger this behavior. If you just want to build a project, run `stack build`.
* stack will *not* track which files it's copied to your local bin path, nor provide a way to automatically delete them. There are many great tools out there for managing installation of binaries, and stack does not attempt to replace those.
* stack will not necessarily be creating a relocatable executable. If your executables hard-codes paths, copying the executable will not change those hard-coded paths. At the time of writing, there's no way to change those kinds of paths with stack, but see [issue #848 about --prefix](https://github.com/commercialhaskell/stack/issues/848) for future plans.

That's really all there is to the install command: for the simplicity of what
it does, it occupies a much larger mental space than is warranted.

## Targets, locals, and extra-deps

We haven't discussed this too much yet, but in addition to having a number of
synonyms, and taking a number of options on the command line, the build command
also takes many arguments. These are parsed in different ways, and can be used
to achieve a high level of flexibility in telling stack exactly what you want
to build.

We're not going to cover the full generality of these arguments here; instead,
there's [a Wiki page covering the full build command
syntax](https://github.com/commercialhaskell/stack/wiki/Build-command).
Instead, let me point out a few different types of arguments:

* You can specify a *package name*, e.g. `stack build vector`. This will attempt to build the vector package, whether it's a local package, in your extra-deps, in your snapshot, or just available upstream. If it's just available upstream but not included in your locals, extra-deps, or snapshot, the newest version is automatically promoted to an extra-dep.
* You can also give a *package identifier*, which is a package name plus version, e.g. `stack build yesod-bin-1.4.14`. This is almost identical to specifying a package name, except it will (1) choose the given version instead of latest, and (2) error out if the given version conflicts with the version of a local package.
* The most flexibility comes from specifying individual *components*, e.g. `stack build helloworld:test:helloworld-test` says "build the test suite component named helloworld-test from the helloworld package." In addition to this long form, you can also shorten it by skipping what type of component it is, e.g. `stack build helloworld:helloworld-test`, or even skip the package name entirely, e.g. `stack build :helloworld-test`.
* Finally, you can specify individual *directories* to build, which will trigger building of any local packages included in those directories or subdirectories.

When you give no specific arguments on the command line (e.g., `stack build`),
it's the same as specifying the names of all of your local packages. If you
just want to build the package for the directory you're currently in, you can
use `stack build .`.

### Components, --test, and --bench

Here's one final important yet subtle point. Consider our helloworld package,
which has a library component, an executable helloworld-exe, and a test suite
helloworld-test. When you run `stack build helloworld`, how does it know which
ones to build? By default, it will build the library (if any) and all of the
executables, but ignore the test suites and benchmarks.

This is where the `--test` and `--bench` flags come into play. If you use them,
those components will also be included. So `stack build --test helloworld` will
end up including the helloworld-test component as well.

You can bypass this implicit adding of components by being much more explicit,
and stating the components directly. For example, the following will not build
the helloworld-exe executable:

```
michael@d30748af6d3d:~/helloworld$ stack clean
michael@d30748af6d3d:~/helloworld$ stack build :helloworld-test
helloworld-0.1.0.0: configure (test)
Configuring helloworld-0.1.0.0...
helloworld-0.1.0.0: build (test)
Preprocessing library helloworld-0.1.0.0...
[1 of 1] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/Lib.o )
In-place registering helloworld-0.1.0.0...
Preprocessing test suite 'helloworld-test' for helloworld-0.1.0.0...
[1 of 1] Compiling Main             ( test/Spec.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-test/helloworld-test-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/helloworld-test/helloworld-test ...
helloworld-0.1.0.0: test (suite: helloworld-test)
Test suite not yet implemented
```

We first cleaned our project to clear old results so we know exactly what stack
is trying to do. Notice that it builds the helloworld-test test suite, and the
helloworld library (since it's used by the test suite), but it does not build
the helloworld-exe executable.

And now the final point: the last line shows that our command also *runs* the
test suite it just built. This may surprise some people who would expect tests
to only be run when using `stack test`, but this design decision is what allows
the `stack build` command to be as composable as it is (as described
previously). The same rule applies to benchmarks. To spell it out completely:

* The --test and --bench flags simply state which components of a package should be built, if no explicit set of components is given
* The default behavior for any test suite or benchmark component which has been built is to also run it

You can use the `--no-run-tests` and `--no-run-benchmarks` (from stack-0.1.4.0
and on) flags to disable running of these components. You can also use
`--no-rerun-tests` to prevent running a test suite which has already passed and
has not changed.

NOTE: stack doesn't build or run test suites and benchmarks for non-local
packages. This is done so that running a command like `stack test` doesn't need
to run 200 test suites!

## Multi-package projects

Until now, everything we've done with stack has used a single-package project.
However, stack's power truly shines when you're working on multi-package
projects. All the functionality you'd expect to work just does: dependencies
between packages are detected and respected, dependencies of all packages are
just as one cohesive whole, and if anything fails to build, the build commands
exits appropriately.

Let's demonstrate this with the wai-app-static and yackage packages:

```
michael@d30748af6d3d:~$ mkdir multi
michael@d30748af6d3d:~$ cd multi/
michael@d30748af6d3d:~/multi$ stack unpack wai-app-static-3.1.1 yackage-0.8.0
wai-app-static-3.1.1: download
Unpacked wai-app-static-3.1.1 to /home/michael/multi/wai-app-static-3.1.1/
Unpacked yackage-0.8.0 to /home/michael/multi/yackage-0.8.0/
michael@d30748af6d3d:~/multi$ stack init
Writing default config file to: /home/michael/multi/stack.yaml
Basing on cabal files:
- /home/michael/multi/yackage-0.8.0/yackage.cabal
- /home/michael/multi/wai-app-static-3.1.1/wai-app-static.cabal

Checking against build plan lts-3.2
Selected resolver: lts-3.2
Wrote project config to: /home/michael/multi/stack.yaml
michael@d30748af6d3d:~/multi$ stack build --haddock --test
# Goes off to build a whole bunch of packages
```

If you look at the stack.yaml, you'll see exactly what you'd expect:

```yaml
flags:
  yackage:
    upload: true
  wai-app-static:
    print: false
packages:
- yackage-0.8.0/
- wai-app-static-3.1.1/
extra-deps: []
resolver: lts-3.2
```

Notice that multiple directories are listed in the `packages` key.

In addition to local directories, you can also refer to packages available in a
Git repository or in a tarball over HTTP/HTTPS. This can be useful for using a
modified version of a dependency that hasn't yet been released upstream. This
is a slightly more advanced usage that we won't go into detail with here, but
it's covered in the [stack.yaml wiki
page](https://github.com/commercialhaskell/stack/wiki/stack.yaml#packages).

## Flags and GHC options

There are two common ways you may wish to alter how a package will install:
with Cabal flags and GHC options. In the stack.yaml file above, you can see
that `stack init` has detected that- for the yackage package- the upload flag
can be set to true, and for wai-app-static, the print flag to false. (The
reason it's chosen those values is that they're the default flag values, and
their dependencies are compatible with the snapshot we're using.)

In order to change this, we can use the command line `--flag` option:

    stack build --flag yackage:-upload

This means: when compiling the yackage package, turn off the upload flag (thus
the `-`). Unlike other tools, stack is explicit about which package's flag you
want to change. It does this for two reasons:

1. There's no global meaning for Cabal flags, and therefore two packages can
   use the same flag name for completely different things.
2. By following this approach, we can avoid unnecessarily recompiling snapshot
   packages that happen to use a flag that we're using.

You can also change flag values on the command line for extra-dep and snapshot
packages. If you do this, that package will automatically be promoted to an
extra-dep, since the build plan is different than what the plan snapshot
definition would entail.

### GHC options

GHC options follow a similar logic, with a few nuances to adjust for common use cases. Let's consider:

    stack build --ghc-options="-Wall -Werror"

This will set the `-Wall -Werror` options for all *local targets*. The
important thing to note here is that it will not affect extra-dep and snapshot
packages at all. This is by design, once again, to get reproducible and fast
builds.

(By the way: that above GHC options have a special convenience flag: `--pedantic`.)

There's one extra nuance about command line GHC options. Since they only apply
to local targets, if you change your local targets, they will no longer apply
to other packages. Let's play around with an example from the wai repository,
which includes the wai and warp packages, the latter depending on the former.
If we run:

    stack build --ghc-options=-O0 wai

It will build all of the dependencies of wai, and then build wai with all
optimizations disabled. Now let's add in warp as well:

    stack build --ghc-options=-O0 wai warp

This builds the additional dependencies for warp, and then builds warp with
optimizations disabled. Importantly: it does not rebuild wai, since wai's
configuration has not been altered. Now the surprising case:

```
michael@d30748af6d3d:~/wai$ stack build --ghc-options=-O0 warp
wai-3.0.3.0-5a49351d03cba6cbaf906972d788e65d: unregistering (flags changed from ["--ghc-options","-O0"] to [])
warp-3.1.3-a91c7c3108f63376877cb3cd5dbe8a7a: unregistering (missing dependencies: wai)
wai-3.0.3.0: configure
```

You may expect this to be a no-op: neither wai nor warp has changed. However,
stack will instead recompile wai with optimizations enabled again, and then
rebuild warp (with optimizations disabled) against this newly built wai. The
reason: reproducible builds. If we'd never built wai or warp before, trying to
build warp would necessitate building all of its dependencies, and it would do
so with default GHC options (optimizations enabled). This dependency would
include wai. So when we run:

    stack build --ghc-options=-O0 warp

We want its behavior to be unaffected by any previous build steps we took.
While this specific corner case does catch people by surprise, the overall goal
of reproducible builds is- in the stack maintainers' views- worth the
confusion.

Final point: if you have GHC options that you'll be regularly passing to your
packages, you can add them to your stack.yaml file (starting with
stack-0.1.4.0). See [the wiki page section on
ghc-options](https://github.com/commercialhaskell/stack/wiki/stack.yaml#ghc-options)
for more information.

## path

NOTE: That's it, the heavy content of this guide is done! Everything from here
on out is simple explanations of commands. Congratulations!

Generally, you don't need to worry about where stack stores various files. But some people like to know this stuff. That's when the `stack path` command is useful.

```
michael@d30748af6d3d:~/wai$ stack path
global-stack-root: /home/michael/.stack
project-root: /home/michael/wai
config-location: /home/michael/wai/stack.yaml
bin-path: /home/michael/.stack/snapshots/x86_64-linux/lts-2.17/7.8.4/bin:/home/michael/.stack/programs/x86_64-linux/ghc-7.8.4/bin:/home/michael/.stack/programs/x86_64-linux/ghc-7.10.2/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ghc-paths: /home/michael/.stack/programs/x86_64-linux
local-bin-path: /home/michael/.local/bin
extra-include-dirs:
extra-library-dirs:
snapshot-pkg-db: /home/michael/.stack/snapshots/x86_64-linux/lts-2.17/7.8.4/pkgdb
local-pkg-db: /home/michael/wai/.stack-work/install/x86_64-linux/lts-2.17/7.8.4/pkgdb
snapshot-install-root: /home/michael/.stack/snapshots/x86_64-linux/lts-2.17/7.8.4
local-install-root: /home/michael/wai/.stack-work/install/x86_64-linux/lts-2.17/7.8.4
snapshot-doc-root: /home/michael/.stack/snapshots/x86_64-linux/lts-2.17/7.8.4/doc
local-doc-root: /home/michael/wai/.stack-work/install/x86_64-linux/lts-2.17/7.8.4/doc
dist-dir: .stack-work/dist/x86_64-linux/Cabal-1.18.1.5
```

In addition, this command accepts command line arguments to state which of
these keys you're interested in, which can be convenient for scripting. As a
simple example, let's find out which versions of GHC are installed locally:

```
michael@d30748af6d3d:~/wai$ ls $(stack path --ghc-paths)/*.installed
/home/michael/.stack/programs/x86_64-linux/ghc-7.10.2.installed
/home/michael/.stack/programs/x86_64-linux/ghc-7.8.4.installed
```

(Yes, that command requires a \*nix shell, and likely won't run on Windows.)

While we're talking about paths, it's worth explaining how to wipe our stack completely. It involves deleting just three things:

1. The stack executable itself
2. The stack root, e.g. `$HOME/.stack` on non-Windows systems. See `stack path --global-stack-root`
    * On Windows, you will also need to delete `stack path --ghc-paths`
3. Any local `.stack-work` directories inside a project

## exec

We've already used `stack exec` used multiple times in this guide. As you've
likely already guessed, it allows you to run executables, but with a slightly
modified environment. In particular: it looks for executables on stack's bin
paths, and sets a few additional environment variables (like
`GHC_PACKAGE_PATH`, which tells GHC which package databases to use). If you
want to see exactly what the modified environment looks like, try:

    stack exec env

The only trick is how to distinguish flags to be passed to stack versus those for the underlying program. Thanks to the optparse-applicative library, stack follows the Unix convention of `--` to separate these, e.g.:

```
michael@d30748af6d3d:~$ stack exec --package stm -- echo I installed the stm package via --package stm
Run from outside a project, using implicit global config
Using latest snapshot resolver: lts-3.2
Writing global (non-project-specific) config file to: /home/michael/.stack/global/stack.yaml
Note: You can change the snapshot via the resolver field there.
I installed the stm package via --package stm
```

Flags worth mentioning:

* `--package foo` can be used to force a package to be installed before running the given command
* `--no-ghc-package-path` can be used to stop the `GHC_PACKAGE_PATH` environment variable from being set. Some tools- notably cabal-install- do not behave well with that variable set

## ghci (the repl)

GHCi is the interactive GHC environment, a.k.a. the REPL. You can access it with:

    stack exec ghci

However, this doesn't do anything particularly intelligent, such as loading up
locally written modules. For that reason, the `stack ghci` command is
available.

NOTE: At the time of writing, `stack ghci` was still an experimental feature,
so I'm not going to devote a lot more time to it. Future readers: feel free to
expand this!

## ghc/runghc

You'll sometimes want to just compile (or run) a single Haskell source file,
instead of creating an entire Cabal package for it. You can use `stack exec
ghc` or `stack exec runghc` for that. As simple helpers, we also provide the
`stack ghc` and `stack runghc` commands, for these common cases.

stack also offers a very useful feature for running files: a script
interpreter. For too long have Haskellers felt shackled to bash or Python
because it's just too hard to create reusable source-only Haskell scripts.
stack attempts to solve that. An example will be easiest to understand:

```
michael@d30748af6d3d:~$ cat turtle.hs
#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Turtle
main = echo "Hello World!"
michael@d30748af6d3d:~$ chmod +x turtle.hs
michael@d30748af6d3d:~$ ./turtle.hs
Run from outside a project, using implicit global config
Using resolver: lts-3.2 specified on command line
hashable-1.2.3.3: configure
# installs some more dependencies
Completed all 22 actions.
Hello World!
michael@d30748af6d3d:~$ ./turtle.hs
Run from outside a project, using implicit global config
Using resolver: lts-3.2 specified on command line
Hello World!
```

If you're on Windows: you can run `stack turtle.hs` instead of `./turtle.hs`.

The first line is the usual "shebang" to use stack as a script interpreter. The
second line, which is required, provides additional options to stack (due to
the common limitation of the "shebang" line only being allowed a single
argument). In this case, the options tell stack to use the lts-3.2 resolver,
automatically install GHC if it is not already installed, and ensure the turtle
package is available.

The first run can take a while, since it has to download GHC and build
dependencies. But subsequent runs are able to reuse everything already built,
and are therefore quite fast.

## Finding project configs, and the implicit global

Whenever you run something with stack, it needs a stack.yaml project file. The
algorithm stack uses to find this is:

1. Check for a `--stack-yaml` option on the command line
2. Check for a `STACK_YAML` environment variable
3. Check the current directory and all ancestor directories for a `stack.yaml` file

The first two provide a convenient method for using an alternate configuration.
For example: `stack build --stack-yaml stack-7.8.yaml` can be used by your CI
system to check your code against GHC 7.8. Setting the `STACK_YAML` environment
variable can be convenient if you're going to be running commands like `stack
ghc` in other directories, but you want to use the configuration you defined in
a specific project.

If stack does not find a stack.yaml in any of the three specified locations,
the *implicit global* logic kicks in. You've probably noticed that phrase a few
times in the output from commands above. Implicit global is essentially a hack
to allow stack to be useful in a non-project setting. When no implicit global
config file exists, stack creates one for you with the latest LTS snapshot as
the resolver. This allows you to do things like:

* compile individual files easily with `stack ghc`
* build executables you'd want to use without starting a project, e.g. `stack install pandoc`

Keep in mind that there's nothing magical about this implicit global
configuration. It has no impact on projects at all, and every package you
install with it is put into isolated databases just like everywhere else. The
only magic is that it's the catch-all project whenever you're running stack
somewhere else.

## stack.yaml vs .cabal files

Now that we've covered a lot of stack use cases, this quick summary of
stack.yaml vs .cabal files will hopefully make a lot of sense, and be a good
reminder for future uses of stack:

* A project can have multiple packages. Each project has a stack.yaml. Each package has a .cabal file
* The .cabal file specifies which packages are dependencies. The stack.yaml file specifies which packages are available to be used
* .cabal specifies the components, modules, and build flags provided by a package
* stack.yaml can override the flag settings for individual packages
* stack.yaml specifies which packages to include

## Comparison to other tools

stack is not the only tool around for building Haskell code. stack came into
existence due to limitations with some of the existing tools. If you're
unaffected by those limitations and are happily building Haskell code, you may
not need stack. If you're suffering from some of the common problems in other
tools, give stack a try instead.

If you're a new user who has no experience with other tools, you should start
with stack. The defaults match modern best practices in Haskell development,
and there are less corner cases you need to be aware of. You *can* develop
Haskell code with other tools, but you probably want to spend your time writing
code, not convincing a tool to do what you want.

Before jumping into the differences, let me clarify an important similarity:

* __Same package format.__ stack, cabal-install, and presumably all other tools share the same underlying Cabal package format, consisting of a .cabal file, modules, etc. This is a Good Thing: we can share the same set of upstream libraries, and collaboratively work on the same project with stack, cabal-install, and NixOS. In that sense, we're sharing the same ecosystem.

Now the differences:

* __Curation vs dependency solving as a default__. stack defaults to using curation (Stackage snapshots, LTS Haskell, Nightly, etc) as a default instead of defaulting to dependency solving, as cabal-install does. This is just a default: as described above, stack can use dependency solving if desired, and cabal-install can use curation. However, most users will stick to the defaults. The stack team firmly believes that the majority of users want to simply ignore dependency resolution nightmares and get a valid build plan from day 1, which is why we've made this selection of default behavior.
* __Reproducible__. stack goes to great lengths to ensure that `stack build` today does the same thing tomorrow. cabal-install does not: build plans can be affected by the presence of preinstalled packages, and running `cabal update` can cause a previously successful build to fail. With stack, changing the build plan is always an explicit decision.
* __Automatically building dependencies__. In cabal-install, you need to use `cabal install` to trigger dependency building. This is somewhat necessary due to the previous point, since building dependencies can in some cases break existing installed packages. So for example, in stack, `stack test` does the same job as `cabal install --run-tests`, though the latter *additionally* performs an installation that you may not want. The closer command equivalent is `cabal install --enable-tests --only-dependencies && cabal configure --enable-tests && cabal build && cabal test` (newer versions of cabal-install may make this command shorter).
* __Isolated by default__. This has been a pain point for new stack users actually. In cabal, the default behavior is a non-isolated build, meaning that working on two projects can cause the user package database to become corrupted. The cabal solution to this is sandboxes. stack, however, provides this behavior by default via its databases. In other words: when you use stack, there's __no need for sandboxes__, everything is (essentially) sandboxed by default.

## More resources

There are lots of resources available for learning more about stack:

* `stack --help`
* `stack --version` can tell you about the version and Git hash of the executable you're using
* `--verbose` (or `-v`) flag give a lot more information about what's going on internally (useful for bug reports)
* The [README](https://github.com/commercialhaskell/stack#readme)
* The [stack mailing list](https://groups.google.com/d/forum/haskell-stack)
* The [stack wiki](https://github.com/commercialhaskell/stack/wiki), and in particular [the FAQ page](https://github.com/commercialhaskell/stack/wiki/FAQ)
* The [haskell-stack tag on Stack Overflow](http://stackoverflow.com/questions/tagged/haskell-stack)

## Fun features

This is just a quick collection of fun and useful feature stack supports.

### Templates

We started off using the `new` command to create a project. stack provides multiple templates to start a new project from:

```
michael@d30748af6d3d:~$ stack templates
chrisdone
hakyll-template
new-template
simple
yesod-minimal
yesod-mongo
yesod-mysql
yesod-postgres
yesod-postgres-fay
yesod-simple
yesod-sqlite
michael@d30748af6d3d:~$ stack new my-yesod-project yesod-simple
Downloading template "yesod-simple" to create project "my-yesod-project" in my-yesod-project/ ...
Using the following authorship configuration:
author-email: example@example.com
author-name: Example Author Name
Copy these to /home/michael/.stack/stack.yaml and edit to use different values.
Writing default config file to: /home/michael/my-yesod-project/stack.yaml
Basing on cabal files:
- /home/michael/my-yesod-project/my-yesod-project.cabal

Checking against build plan lts-3.2
Selected resolver: lts-3.2
Wrote project config to: /home/michael/my-yesod-project/stack.yaml
```

To add more templates, see [the stack-templates
repository](https://github.com/commercialhaskell/stack-templates#readme).

### IDE

stack has a work-in-progress suite of editor integrations, to do things like
getting type information in emacs. For more information, see
[stack-ide](https://github.com/commercialhaskell/stack-ide#readme).

### Visualizing dependencies

If you'd like to get some insight into the dependency tree of your packages,
you can use the `stack dot` command and Graphviz. More information is
[available on the
wiki](https://github.com/commercialhaskell/stack/wiki/Visualizing-dependencies-with-Graphviz).

### Travis with caching

Many people use Travis CI to test out a project for every Git push. We have [a
Wiki page devoted to
Travis](https://github.com/commercialhaskell/stack/wiki/Travis). However, for
most people, the following example will be sufficient to get started:

```yaml
sudo: false
language: c

addons:
  apt:
    packages:
    - libgmp-dev

before_install:
# stack
- mkdir -p ~/.local/bin
- export PATH=$HOME/.local/bin:$PATH
- travis_retry curl -L https://github.com/commercialhaskell/stack/releases/download/v0.1.3.1/stack-0.1.3.1-x86_64-linux.gz | gunzip > ~/.local/bin/stack
- chmod a+x ~/.local/bin/stack

script:
- stack --no-terminal setup
- stack --no-terminal build
- stack --no-terminal test

cache:
  directories:
  - $HOME/.stack
```

Not only will this build and test your project, but it will cache your snapshot
built packages, meaning that subsequent builds will be much faster.

Two notes for future improvement:

* One Travis whitelists the stack .deb files, we'll be able to simply include stack in the `addons` section, and automatically use the newest version of stack, avoiding that complicated `before_install` section
* Starting with stack-0.1.4.0, there are improvements to the test command, so that the entire script section can be `stack --no-terminal --install-ghc test`

If you're wondering: the reason we need `--no-terminal` is because stack does
some fancy sticky display on smart terminals to give nicer status and progress
messages, and the terminal detection is broken on Travis.

### Shell autocompletion

Love being able to tab-complete commands? You're not alone. If you're on bash, just run the following (or add it to `.bashrc`):

    eval "$(stack --bash-completion-script "$(which stack)")"

For more information and other shells, see [the Shell autocompletion wiki page](https://github.com/commercialhaskell/stack/wiki/Shell-autocompletion)

### Docker

stack provides two built-in Docker integrations. Firstly, you can build your code inside a Docker image, which means:

* even more reproducibility to your builds, since you and the rest of your team will always have the same system libraries
* the Docker images ship with entire precompiled snapshots. That means you have a large initial download, but much faster builds

For more information, see [the Docker wiki page](https://github.com/commercialhaskell/stack/wiki/Docker).

The other integration is that stack can generate Docker images for you
containing your built executables. This feature is great for automating
deployments from CI. This feature is not yet very well documented, but the
basics are to add a section like the following to stack.yaml:

```yaml
image:
  container:
    base: "fpco/ubuntu-with-libgmp:14.04"
    add:
      man/: /usr/local/share/man/
    entrypoints:
      - stack
```

and then run `stack image container`.

## Power user commands

The following commands are a little more powerful, and therefore won't be
needed by all users. Here's a quick rundown:

* `stack update` will download the most recent set of packages from your package indices (e.g. Hackage). Generally, stack runs this for you automatically when necessary, but it can be useful to do this manually sometimes (e.g., before running `stack solver`, to guarantee you have the most recent upstream packages available).
* `stack unpack` is a command we've already used quite a bit for examples, but most users won't use it regularly. It does what you'd expect: downloads a tarball and unpacks it.
* `stack sdist` generates an uploading tarball containing your package code
* `stack upload` uploads an sdist to Hackage. In the future, it will also perform automatic GPG signing of your packages for additional security, when configured.
* `stack upgrade` will build a new version of stack from source. `--git` is a convenient way to get the most recent version from master for those testing and living on the bleeding edge.
* `stack setup --upgrade-cabal` can install a newer version of the Cabal library, used for performing actual builds. You shouldn't generally do this, since new Cabal versions may introduce incompatibilities with package sets, but it can be useful if you're trying to test a specific bugfix.
* `stack list-dependencies` lists all of the packages and versions used for a project
