stack is a cross-platform programm for developing Haskell projects. This guide
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

That was a bit anticlimatic. The problem is that stack needs GHC in order to
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

## The setup command

* stack setup (nothing happens)

## The build command

## Adding dependencies

* To cabal file
* Outside of package set

## Curated package sets

* stackage.org
    * Resolver names
    * Hoogle

## Resolvers and changing your compiler version

## Existing projects

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
