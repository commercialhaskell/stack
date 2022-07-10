<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Nix integration

(since 0.1.10.0)

When using the Nix integration, Stack handles Haskell dependencies as usual
while Nix handles _non-Haskell_ dependencies needed by these Haskell packages.
So Stack downloads Haskell packages from [Stackage](https://www.stackage.org/lts)
and builds them locally but uses Nix to download 
[Nix packages][nix-search-packages] that provide the GHC compiler and 
external C libraries that you would normally install manually.
You can install Nix with all the necessary commandline tools from the 
[Nix download page](http://nixos.org/nix/download.html).

`stack` can automatically create a Nix build environment in the background
using `nix-shell`, similar to building inside an isolated 
[Docker](https://www.docker.com/) container. 
There are two options to create such a build environment:

- provide a list of [Nix packages][nix-search-packages]
- provide a custom `shell.nix` file that gives you more control 
  of what libraries and tools are available inside the shell. 

The second requires writing code in 
[Nix's custom language][nix-language]. 
So use this option only if you already know Nix and have special requirements,
such as using custom Nix packages that override the standard ones or
using system libraries with special requirements.

### Checking Nix installation

Follow the instructions on the
[Nix download page](http://nixos.org/nix/download.html) to install Nix.  After
doing so, when opening a terminal, the nix commands (`nix-build`, `nix-shell`,
etc) should be available.  If they are not, it should be because the file
located at `$HOME/.nix-profile/etc/profile.d/nix.sh` is not sourced by your shell.

You should either run `source ~/.nix-profile/etc/profile.d/nix.sh` manually
every time you open a terminal and need Nix or add this command to your
`~/.bashrc` or `~/.bash_profile`.

### External C libraries through Nix packages

To let Nix manage external C libraries by default
add the following section to your `stack.yaml` file:
```yaml
nix:
  enable: true
  packages: [zlib, glpk, pcre]
```

This will instruct `stack` to build inside a local build environment
that will have the Nix packages 
[zlib](https://search.nixos.org/packages?query=zlib),
[glpk](https://search.nixos.org/packages?query=glpk) and
[pcre](https://search.nixos.org/packages?query=pcre)
installed, which provide the C libraries of the same names. 
Further, the build environment will implicitly also download and use
a [GHC Nix package](https://search.nixos.org/packages?query=haskell.compiler.ghc)
matching the required version of the configured 
[Stack resolver](https://docs.haskellstack.org/en/stable/GUIDE/#resolvers-and-changing-your-compiler-version). 
So, enabling Nix support means that packages will always be built using the 
local GHC from Nix inside your shell, rather than your 
globally installed system GHC if any.

Note that *in this mode every developer of your project needs to have Nix installed*, 
but also gets all external libraries automatically*. Quite convenient.
If some developers don't have or want Nix, there's a nice tutorial on 
[how to add Nix integration optionally](https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/).

Also note that `stack` can use only GHC versions that have
already been mirrored into the Nix package repository.
The [Nixpkgs master branch](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/haskell-modules)
usually picks up new versions quickly, but it takes two or three
days before those updates arrive in the `unstable` channel. Release
channels, like `nixos-22.05`, receive those updates only
occasionally -- say, every two or three months --, so you should not
expect them to have the latest compiler available. Fresh NixOS installs
use a release version by default.

To know for sure whether a given compiler is available on your system,
you can use the Nix command

```sh
$ nix-env -f "<nixpkgs>" -qaP -A haskell.compiler.ghc801
haskell.compiler.ghc801  ghc-8.0.1
```

to check whether it's available. If Nix doesn't know that resolver
yet, then you'll see the following error message instead:

```sh
$ nix-env -f "<nixpkgs>" -qaP -A haskell.compiler.ghc999
error: attribute ‘ghc999’ in selection path ‘haskell.compiler.ghc999’ not found
```

You can list all known Haskell compilers in Nix with the following:

```sh
$ nix-instantiate --eval -E "with import <nixpkgs> {}; lib.attrNames haskell.compiler"
```

Alternatively, use `nix repl`, a convenient tool to explore
nixpkgs:

```sh
$ nix repl
```

In the REPL, load nixpkgs and get the same information through
autocomplete:

```sh
nix-repl> :l <nixpkgs>
nix-repl> haskell.compiler.ghc<Tab>
```

You can type and evaluate any nix expression in the nix repl, such as
the one we gave to `nix-instantiate` earlier.

**Note:** currently, stack only discovers dynamic and static libraries
in the `lib/` folder of any nix package, and likewise header files in
the `include/` folder. If you're dealing with a package that doesn't
follow this standard layout, you'll have to deal with that using
a custom shell file (see below).

### Using Stack with Nix enabled

With Nix enabled, `stack build` and `stack exec` will automatically
launch themselves in a local build environment (using `nix-shell`
behind the scenes).

`stack setup` will start a nix-shell, so it will gather all the required
packages, but given nix handles GHC installation, instead of stack, this will
happen when running `stack build` if no setup has been performed
before. Therefore it is no longer necessary to run `stack setup` unless you
want to cache a GHC installation before running the build.

If `enable:` is omitted or set to `false` in your `stack.yaml` file, 
you can still build within a nix-shell by
overriding Stack through the `--nix` flag, for instance `stack --nix build`.  
Passing any `--nix*` option to the command line will do the same.

**Known limitation on macOS:** currently, `stack --nix ghci` fails on
macOS, due to a bug in GHCi when working with external shared
libraries.

### Pure and impure Nix shell

By default, stack will run the build in a *pure* Nix build environment (or
*shell*), which means two important things:

- basically **no environment variable will be forwarded** from your user session
  to the nix-shell (variables like `HTTP_PROXY` or `PATH` notably will not be
  available),
- the build should fail if you haven't specified all the dependencies in the
  `packages:` section of the `stack.yaml` file, even if these dependencies are
  installed elsewhere on your system. This behaviour enforces a complete
  description of the build environment to facilitate reproducibility.

To override this behaviour, add `pure: false` to your `stack.yaml` or pass the
`--no-nix-pure` option to the command line.

**Note:** On macOS shells are non-pure by default currently. This is
due soon to be resolved locale issues. So on macOS you'll need to be
a bit more careful to check that you really have listed all
dependencies.

### Nix package sources

Nix organizes its packages in snapshots of packages (each snapshot being
a  "package set")
similar to how Stackage organizes Haskell packages. 
By default, `nix-shell` will look for the "nixpkgs" package set located
by your `NIX_PATH` environment variable. This package set can be different
depending on when you installed Nix and which nixpkgs channel you're using 
(similar to the LTS channel for stable packages and the nightly channel for bleeding
edge packages in [Stackage](https://www.stackage.org/)).
This is bad for reproducibilty so that nixpkgs should be pinned, i.e.,
set to the same package set for every developer of your project.

You can set or override the Nix package set by passing
`--nix-path="nixpkgs=/my/own/nixpkgs/clone"` to ask Nix to use your
own local checkout of the nixpkgs repository. You could in this way
use a bleeding edge nixpkgs, cloned from the
[nixpkgs](http://www.github.com/NixOS/nixpkgs) `master` branch, or
edit the nix descriptions of some packages. Setting

```yml
nix:
  path: [nixpkgs=/my/own/nixpkgs/clone]
```

in your `stack.yaml` will do the same. 
[This example repository](https://github.com/tweag/haskell-stack-nix-example)
shows how you can pin a package set.

## Command-line options

The configuration present in your `stack.yaml` can be overridden on the
command-line. See `stack --nix-help` for a list of all Nix options.

## Configuration options

`stack.yaml` contains a `nix:` section for Nix settings.
Without this section, Nix won't be used.

Here's a working configuration file with all settings, mentioning default values:

```yaml
nix:

  # false by default. Must be present and set to `true` to enable Nix, except on
  # NixOS where it is enabled by default (see #3938).  You can set set it in your
  # `$HOME/.stack/config.yaml` to enable Nix for all your projects without having
  # to repeat it
  enable: true

  # true by default. Tells Nix whether to run in a pure shell or not.
  pure: true

  # Empty by default. The list of packages you want to be
  # available in the nix-shell at build time (with `stack
  # build`) and run time (with `stack exec`).
  packages: []

  # Unset by default. You cannot set this option if `packages:`
  # is already present and not empty.
  shell-file: shell.nix

  # A list of strings, empty by default. Additional options that
  # will be passed verbatim to the `nix-shell` command.
  nix-shell-options: []

  # A list of strings, empty by default, such as
  # `[nixpkgs=/my/local/nixpkgs/clone]` that will be used to override
  # NIX_PATH.
  path: []
  
  # false by default. Whether to add your nix dependencies as nix garbage
  # collection roots. This way, calling nix-collect-garbage will not remove
  # those packages from the nix store, saving you some time when running
  # stack build again with nix support activated.
  # This creates a `nix-gc-symlinks` directory in the project `.stack-work`.
  # To revert that, just delete this `nix-gc-symlinks` directory.
  add-gc-roots: false
```

## External C libraries through shell.nix

There's also the [Nix programming language][nix-language]
to provide a fully customized derivation as an environment to use. 
Here's the equivalent of the configuration used in the 
[previous example](#external-c-libraries-through-nix-packages), 
but with an explicit `shell.nix` file 
(make sure you're using a nixpkgs version later than 2015-03-05):

```nix
{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ zlib glpk pcre ];
}
```

Defining a `shell.nix` file manually gives you the possibility to override some
Nix derivations ("packages"), for instance to change some build options of the
libraries you use, or to set additional environment variables. See the
[Nix manual][nix-manual-exprs] for more. The `buildStackProject` utility
function is documented in the [Nixpkgs manual][nixpkgs-manual-haskell].  In such
case, stack expect this file to define a function of exactly one argument that
should be called `ghc` (as arguments within a set are non-positional), which you
should give to `buildStackProject`. This is a GHC Nix package in the version as
defined in the resolver you set in the `stack.yaml` file.

And now for the `stack.yaml` file:

```yaml
nix:
  enable: true
  shell-file: shell.nix
```

The `stack build` command will behave exactly the same as above. Note
that specifying both `packages:` and a `shell-file:` results in an
error. (Comment one out before adding the other.)

[nix-manual-exprs]: http://nixos.org/nix/manual/#chap-writing-nix-expressions
[nixpkgs-manual-haskell]: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure
[nix-search-packages]: https://nixos.org/nixos/packages.html
[nix-language]: https://nixos.wiki/wiki/Nix_Expression_Language

