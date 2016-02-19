# FAQ

So that this doesn't become repetitive: for the reasons behind the answers
below, see the [Architecture](architecture.md) page. The goal of the answers here is to be as
helpful and concise as possible.

#### Where is stack installed and will it interfere with `ghc` (etc) I already have installed?

Stack itself is installed in normal system locations based on the mechanism you used (see the [Install and upgrade](install_and_upgrade.md) page). Stack installs the Stackage libraries in `~/.stack` and any project libraries or extra dependencies in a `.stack-work` directory within each project's directory. None of this should affect any existing Haskell tools at all.

#### What is the relationship between stack and cabal?

* Cabal-the-library is used by stack to build your Haskell code.
* cabal-install (the executable) is used by stack for its dependency solver functionality.
* A .cabal file is provided for each package, and defines all package-level metadata just like it does in the cabal-install world: modules, executables, test suites, etc. No change at all on this front.
* A stack.yaml file references 1 or more packages, and provides information on where dependencies come from.
* `stack build` currently initializes a stack.yaml from the existing .cabal file. Project initialization is something that is still being discussed and there may be more options here for new projects in the future (see issue [253](https://github.com/commercialhaskell/stack/issues/253))

#### I need to use a different version of a package than what is provided by the LTS Haskell snapshot I'm using, what should I do?

You can make tweaks to a snapshot by modifying the `extra-deps` configuration value in your `stack.yaml` file, e.g.:

```yaml
resolver: lts-2.9
packages:
- '.'
extra-deps:
- text-1.2.1.1
```

#### I need to use a package (or version of a package) that is not available on hackage, what should I do?

Add it to the `packages` list in your project's `stack.yaml`, specifying the package's source code location relative to the directory where your `stack.yaml` file lives, e.g.

```yaml
resolver: lts-2.10
packages:
- '.'
- third-party/proprietary-dep
- github-version-of/conduit
- patched/diagrams
extra-deps: []
```

The above example specifies that the `proprietary-dep` package is found in the project's `third-party` folder, that the `conduit` package is found in the project's `github-version-of` folder, and that the `diagrams` package is found in the project's `patched` folder. This autodetects changes and reinstalls the package.

To install packages directly from a Git repository, use e.g.:

```yaml
resolver: lts-2.10
packages:
- location:
    git: https://github.com/githubuser/reponame.git
    commit: somecommitID
```

#### What is the meaning of the arguments given to stack build, test, etc?

Those are the targets of the build, and can have one of three formats:

* A package name (e.g., `my-package`) will mean that the `my-package` package must be built
* A package identifier (e.g., `my-package-1.2.3`), which includes a specific version. This is useful for passing to `stack install` for getting a specific version from upstream
* A directory (e.g., `./my-package`) for including a local directory's package, including any packages in subdirectories

#### I need to modify an upstream package, how should I do it?

Typically, you will want to get the source for the package and then add it to
your `packages` list in stack.yaml. (See the previous question.)
`stack unpack` is one approach for getting the source.
Another would be to add the upstream package as a submodule to your
project.

#### Am I required to use a Stackage snapshot to use stack?

No, not at all. If you prefer dependency solving to curation, you can continue with that workflow. Instead of describing the details of how that works here, it's probably easiest to just say: run `stack init --solver` and look at the generated stack.yaml.

#### How do I use this with sandboxes?

Explicit sandboxing on the part of the user is not required by stack. All
builds are automatically isolated into separate package databases without any
user interaction. This ensures that you won't accidentally corrupt your
installed packages with actions taken in other projects.

#### Can I run `cabal` commands inside `stack exec`?

With a recent enough version of cabal-install (>= 1.22), you can. For older versions, due to [haskell/cabal#1800](https://github.com/haskell/cabal/issues/1800), this does not work. Note that even with recent versions, for some commands you may need this extra level of indirection:
```
$ stack exec -- cabal exec -- cabal <command>
```

However, virtually all `cabal` commands have an equivalent in stack, so this should not be necessary. In particular, `cabal` users may be accustomed to the `cabal run` command. In stack:
```
$ stack build && stack exec <program-name>
````
Or, if you want to install the binaries in a shared location:
```
$ stack install
$ <program-name>
```
assuming your `$PATH` has been set appropriately.

#### Using custom preprocessors

If you have a custom preprocessor, for example, Ruby, you may have a
file like:

***B.erb***

``` haskell
module B where

<% (1..5).each do |i| %>
test<%= i %> :: Int
test<%= i %> = <%= i %>
<% end %>
```

To ensure that Stack picks up changes to this file for rebuilds, add
the following line to your .cabal file:

    extra-source-files:   B.erb

#### I already have GHC installed, can I still use stack?

Yes. stack will default to using whatever GHC is on your `PATH`. If that GHC is a
compatible version with the snapshot you're using, it will simply use it.
Otherwise, it will prompt you to run `stack setup`. Note that `stack setup` installs GHC into `~/.stack/programs/$platform/ghc-$version/` and not a global location.

Note that GHC installation doesn't work for all OSes, so in some cases the
first option will need to install GHC yourself.

#### How does stack determine what GHC to use?

It uses the first GHC that it finds on the `PATH`. If that GHC does not comply with the various requirements (version, architecture) that your project needs, it will prompt you to run `stack setup` to get it. `stack` is fully aware of all GHCs that it has installed itself.

See [this issue](https://github.com/commercialhaskell/stack/issues/420) for a detailed discussion.

#### How do I upgrade to GHC 7.10.2 with stack?

If you already have a prior version of GHC use `stack --resolver ghc-7.10 setup --reinstall`. If you don't have any GHC installed, you can skip the `--reinstall`. 

#### How do I get extra build tools?

stack will automatically install build tools required by your packages or their
dependencies, in particular alex and happy.

__NOTE__: This works when using lts or nightly resolvers, not with ghc or custom resolvers. You can manually install build tools by running, e.g., `stack build alex happy`.

#### How does stack choose which snapshot to use when creating a new config file?

It checks the two most recent LTS Haskell major versions and the most recent
Stackage Nightly for a snapshot that is compatible with all of the version
bounds in your .cabal file, favoring the most recent LTS. For more information,
see the snapshot auto-detection section in the architecture document.

#### I'd like to use my installed packages in a different directory. How do I tell stack where to find my packages?

Set the `STACK_YAML` environment variable to point to the `stack.yaml` config
file for your project. Then you can run `stack exec`, `stack ghc`, etc., from
any directory and still use your packages.

#### My tests are failing. What should I do?

Like all other targets, `stack test` runs test suites in parallel by default. This can cause problems with test suites that depend on global resources such as a database or binding to a fixed port number. A quick hack is to force stack to run all test suites in sequence, using `stack test --jobs=1`. For test suites to run in parallel developers should ensure that their test suites do not depend on global resources (e.g. by asking the OS for a random port to bind to) and where unavoidable, add a lock in order to serialize access to shared resources.

#### Can I get bash autocompletion?

Yes, see the [shell-autocompletion documentation](shell_autocompletion.md)

#### How do I update my package index?

Users of cabal are used to running `cabal update` regularly. You can do the
same with stack by running `stack update`. But generally, it's not necessary:
if the package index is missing, or if a snapshot refers to package/version
that isn't available, stack will automatically update and then try again. If
you run into a situation where stack doesn't automatically do the update for
you, please report it as a bug.

#### Isn't it dangerous to automatically update the index? Can't that corrupt build plans?

No, stack is very explicit about which packages it's going to build for you.
There are three sources of information to tell it which packages to install:
the selected snapshot, the `extra-deps` configuration value, and your local
packages. The only way to get stack to change its build plan is to modify one
of those three. Updating the index will have no impact on stack's behavior.

#### I have a custom package index I'd like to use, how do I do so?

You can configure this in your stack.yaml. See [YAML configuration](yaml_configuration.md).

#### How can I make sure my project builds against multiple ghc versions?

You can create multiple yaml files for your project,
one for each build plan. For example, you might set up your project directory like so:

```
myproject/
  stack-7.8.yaml
  stack-7.10.yaml
  stack.yaml --> symlink to stack-7.8.yaml
  myproject.cabal
  src/
    ...
```

When you run `stack build`, you can set the
`STACK_YAML` environment variable to indicate which build plan to use.

```
$ stack build                             # builds using the default stack.yaml
$ STACK_YAML=stack-7.10.yaml stack build  # builds using the given yaml file
```

#### I heard you can use this with Docker?

Yes, stack supports using Docker with images that contain preinstalled Stackage
packages and the tools. See [Docker integration](docker_integration.md) for details.

#### How do I use this with Travis CI?

See the [Travis section in the GUIDE](GUIDE.md#travis-with-caching)

#### What is licensing restrictions on Windows?

Currently on Windows GHC produces binaries linked statically with [GNU Multiple Precision Arithmetic Library](https://gmplib.org/) (GMP), which is used by [integer-gmp](https://hackage.haskell.org/package/integer-gmp) library to provide big integer implementation for Haskell. Contrary to the majority of Haskell code licensed under permissive BSD3 license, GMP library is licensed under LGPL, which means resulting binaries [have to be provided with source code or object files](http://www.gnu.org/licenses/gpl-faq.html#LGPLStaticVsDynamic). That may or may not be acceptable for your situation. Current workaround is to use GHC built with alternative big integer implementation called integer-simple, which is free from LGPL limitations as it's pure Haskell and does not use GMP. Unfortunately it has yet to be available out of the box with stack. See [issue #399](https://github.com/commercialhaskell/stack/issues/399) for the ongoing effort and information on workarounds.

#### How to get a working executable on Windows?

When executing a binary after building with `stack build` (e.g. for target "foo"), the command `foo.exe` might complain about missing runtime libraries (whereas `stack exec foo` works).

Windows is not able to find the necessary C++ libraries from the standard prompt because they're not in the PATH environment variable. `stack exec` works because it's modifying PATH to include extra things.

Those libraries are shipped with GHC (and, theoretically in some cases, MSYS). The easiest way to find them is `stack exec which`. E.g.

    >stack exec which libstdc++-6.dll
    /c/Users/Michael/AppData/Local/Programs/stack/i386-windows/ghc-7.8.4/mingw/bin/libstdc++-6.dll

A quick workaround is adding this path to the PATH environment variable or copying the files somewhere Windows finds them (cf. https://msdn.microsoft.com/de-de/library/7d83bc18.aspx).

Cf. issue [#425](https://github.com/commercialhaskell/stack/issues/425).

#### Can I change stack's default temporary directory?

Stack makes use of a temporary directory for some commands (/tmp by default on linux). If there is not enough free space in this directory, stack may fail (see issue [#429](https://github.com/commercialhaskell/stack/issues/429) ). For instance `stack setup` with a GHC installation requires roughly 1GB free.

A custom temporary directory can be forced:
* on Linux by setting the environment variable TMPDIR (eg `$ TMPDIR=path-to-tmp stack setup`)
* on Windows by setting one of the environment variable (given in priority order), TMP, TEMP, USERPROFILE

If you use Stack with Nix integration, be aware that Nix _also_ uses that TMPDIR
variable, and if it is not set Nix sets it to some subdirectory of `/run`, which
on most Linuxes is a Ramdir. Nix will run the builds in TMPDIR, therefore if you
don't have enough RAM you will get errors about disk space.  If this happens to
you, please _manually_ set TMPDIR before launching Stack to some directory on the
disk.

#### stack sometimes rebuilds based on flag changes when I wouldn't expect it to. How come?

stack tries to give you reproducibility whenever possible. In some cases, this means that you get a recompile when one may not seem necessary. The most common example is running something like this in a multi-package project:

    stack build --ghc-options -O0 && stack build --ghc-options -O0 one-of-the-packages

This may end up recompiling local dependencies of `one-of-the-packages` without optimizations on. Whether stack should or shouldn't do this depends on the needs of the user at the time, and unfortunately we can't make a solution that will make everyone happy in all cases. If you're curious for details, there's [a long discussion about it](https://github.com/commercialhaskell/stack/issues/382) on the issue tracker.

#### stack setup on a windows system only tells me to add certain paths to the PATH variable instead of doing it

If you are using a powershell session, it is easy to automate even that step:

    $env:Path = ( stack setup | %{ $_ -replace '[^ ]+ ', ''} ), $env:Path -join ";"

#### How do I reset / remove Stack (such as to to do a completely fresh build)?

The first thing to remove is project-specific `.stack-work` directory within the project's directory. Next, remove `~/.stack` directory overall. You may have errors if you remove the latter but leave the former. Removing Stack itself will relate to how it was installed, and if you used GHC installed outside of Stack, that would need to be removed separately.

#### How does stack handle parallel builds? What exactly does it run in parallel?

See [issue #644](https://github.com/commercialhaskell/stack/issues/644) for more details.

#### I get strange `ld` errors about recompiling with "-fPIC"

Some users (myself included!) have come across a linker errors (example below) that seem to be dependent on the local environment, i.e. the package may compile on a different machine. There is no known workaround (if you come across one please include details), however the issue has been reported to be [non-deterministic](https://github.com/commercialhaskell/stack/issues/614)
in some cases. I've had success using the docker functionality to build the project on a machine that would not compile it otherwise.

```
tmp-0.1.0.0: build
Building tmp-0.1.0.0...
Preprocessing executable 'tmp' for tmp-0.1.0.0...
Linking dist-stack/x86_64-linux/Cabal-1.22.2.0/build/tmp/tmp ...
/usr/bin/ld: dist-stack/x86_64-linux/Cabal-1.22.2.0/build/tmp/tmp-tmp/Main.o: relocation R_X86_64_32S against `stg_bh_upd_frame_info' can not be used when making a shared object; recompile with -fPIC
dist-stack/x86_64-linux/Cabal-1.22.2.0/build/tmp/tmp-tmp/Main.o: error adding symbols: Bad value
collect2: error: ld returned 1 exit status

--  While building package tmp-0.1.0.0 using:
      /home/philip/.stack/programs/x86_64-linux/ghc-7.10.1/bin/runghc-7.10.1 -package=Cabal-1.22.2.0 -clear-package-db -global-package-db /home/philip/tmp/Setup.hs --builddir=dist-stack/x86_64-linux/Cabal-1.22.2.0/ build
    Process exited with code: ExitFailure 1
```

#### Where does the output from `--ghc-options=-ddump-splices` (and other `-ddump*` options) go?

These are written to `*.dump-*` files inside the package's `.stack-work` directory.

#### Why is DYLD_LIBRARY_PATH ignored?

<a name="dyld-library-path-ignored"></a>If you
are on Mac OS X 10.11 ("El Capitan") or later, there are upstream issues which
[prevent the `DYLD_LIBRARY_PATH` environment variable from being passed to GHC](https://github.com/commercialhaskell/stack/issues/1161)
when System Integrity Protection (a.k.a. "rootless") is enabled. The only
workaround we are aware of is
[disabling System Integrity Protection](http://osxdaily.com/2015/10/05/disable-rootless-system-integrity-protection-mac-os-x/).

**WARNING: Disabling SIP will severely reduce the security of your system, so only do this if absolutely necessary!**

#### Why do I get a `/usr/bin/ar: permission denied` error?

<a name="usr-bin-ar-permission-denied"></a>If you are on OS X 10.11 ("El Capitan") or
later, GHC 7.8.4 is
[incompatible with System Integrity Protection (a.k.a. "rootless")](https://github.com/commercialhaskell/stack/issues/563).
GHC 7.10.2 includes a fix, so this only effects users of GHC 7.8.4. If you
cannot upgrade to GHC 7.10.2, you can work around it by
[disabling System Integrity Protection](http://osxdaily.com/2015/10/05/disable-rootless-system-integrity-protection-mac-os-x/).

**WARNING: Disabling SIP will severely reduce the security of your system, so only do this if absolutely necessary!**

#### Why is the `--` argument separator ignored in Windows PowerShell

Some versions of Windows PowerShell
[don't pass the `--` to programs](https://github.com/commercialhaskell/stack/issues/813).
The workaround is to quote the `"--"`, e.g.:

    stack exec "--" cabal --version

This is known to be a problem on Windows 7, but seems to be fixed on Windows 10.
