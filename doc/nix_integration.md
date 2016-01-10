# Nix integration

(since 0.1.10.0)

`stack` can automatically create a build environment (the equivalent
of a "container" in Docker parlance) using `nix-shell`, provided Nix
is already installed on your system. To do so, please visit the
[Nix download page](http://nixos.org/nix/download.html).

There are two ways to create a build environment:

- providing a list of packages (by "attribute name") from
  [Nixpkgs](http://nixos.org/nixos/packages.html), or
- providing a custom `shell.nix` file containing a Nix expression that
  determines a *derivation*, i.e. a specification of what resources
  are available inside the shell.

The second requires writing code in Nix's custom language. So use this
option only if you already know Nix and have special requirements,
such as using custom Nix packages that override the standard ones or
using system libraries with special requirements.

### Additions to your `stack.yaml`

Add a section to your `stack.yaml` as follows:
```yaml
nix:
  enable: true
  packages: [glpk, pcre]
```

This will instruct `stack` to build inside a local build environment
that will have the `glpk` and `pcre` libraries installed and
available. Further, the build environment will implicitly also include
a version of GHC matching the configured resolver. Enabling Nix
support means packages will always be built using a GHC available
inside the shell, rather than your globally installed one if any.

Note that in this mode `stack` can use only those resolvers that have
already been mirrored into the Nix package repository. The
[Nixpkgs master branch](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/haskell-modules)
usually picks up new resolvers such as Stackage nightlies and LTS
versions within two or three days. Then it takes another two or three
days before those updates arrive in the `unstable` channel. Release
channels, like `nixos-15.09`, receive those updates only
occasionally -- say, every two or three months --, so you should not
expect them to have the latest resolvers available. Fresh Nix installs
use a release version by default.

To know for sure whether a given resolver as available on your system,
you can use the command

```sh
$ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.lts-3_13.ghc
haskell.packages.lts-3_13.ghc  ghc-7.10.2
```

to check whether it's available. If Nix doesn't know that resolver
yet, then you'll see the following error message instead:

```sh
$ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.lts-3_99.ghc
error: attribute ‘lts-3_99’ in selection path ‘haskell.packages.lts-3_99.ghc’ not found
```

You can list all known Haskell package sets in Nix with the following:

```sh
$ nix-instantiate --eval -E "with import <nixpkgs> {}; lib.attrNames haskell.packages"
```

Alternatively, install `nix-repl`, a convenient tool to explore
nixpkgs:

```sh
$ nix-env -i nix-repl
$ nix-repl
```

In the REPL, load nixpkgs and get the same information through
autocomplete:

```sh
nix-repl> :l <nixpkgs>
nix-repl> haskell.packages.lts-<Tab>
```

You can type and evaluate any nix expression in the nix-repl, such as
the one we gave to `nix-instantiate` earlier.

**Note:** currently, stack only discovers dynamic and static libraries
in the `lib/` folder of any nix package, and likewise header files in
the `include/` folder. If you're dealing with a package that doesn't
follow this standard layout, you'll have to deal with that using
a custom shell file (see below).

### Use stack as normal

With Nix enabled, `stack build` and `stack exec` will automatically
launch themselves in a local build environment (using `nix-shell`
behind the scenes).

If `enable:` is set to `false`, you can still build in a nix-shell by
passing the `--nix` flag to stack, for instance `stack --nix build`.
Passing any `--nix*` option to the command line will do the same.

**Known limitation on OS X:** currently, `stack --nix ghci` fails on
OS X, due to a bug in GHCi when working with external shared
libraries.

### The Nix shell

By default, stack will run the build in a pure Nix build environment
(or *shell*), which means the build should fail if you haven't
specified all the dependencies in the `packages:` section of the
`stack.yaml` file, even if these dependencies are installed elsewhere
on your system. This behaviour enforces a complete description of the
build environment to facilitate reproducibility. To override this
behaviour, add `pure: false` to your `stack.yaml` or pass the
`--no-nix-pure` option to the command line.

**Note:** On OS X shells are non-pure by default currently. This is
due soon to be resolved locale issues. So on OS X you'll need to be
a bit more careful to check that you really have listed all
dependencies.

### Package sources

By default, `nix-shell` will look for the nixpkgs package set located
by your `NIX_PATH` environment variable.

You can override this by passing
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

## Command-line options

The configuration present in your `stack.yaml` can be overridden on the
command-line. See `stack --nix-help` for a list of all Nix options.

## Configuration

`stack.yaml` contains a `nix:` section with Nix settings.
Without this section, Nix will not be used.

Here is a commented configuration file, showing the default values:

```yaml
nix:

  # true by default when the nix section is present. Set
  # it to `false` to disable using Nix.
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
```

## Using a custom shell.nix file

Nix is also a programming language, and as specified
[here](#nix-integration) if you know it you can provide to the shell
a fully customized derivation as an environment to use. Here is the
equivalent of the configuration used in
[this section](#additions-to-your-stackyaml), but with an explicit
`shell.nix` file:

```nix
with (import <nixpkgs> {});

stdenv.mkDerivation {

  name = "myEnv";
  
  buildInputs = [
    glpk 
    pcre 
    haskell.packages.lts-3_13.ghc
  ];
  
  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${glpk}/lib" 
      + " --extra-include-dirs=${glpk}/include" 
      + " --extra-lib-dirs=${pcre}/lib" 
      + " --extra-include-dirs=${pcre}/include"
  ;
}
```

Note that in this case, you _have_ to include (a version of) GHC in
your `buildInputs`! This potentially allows you to use a GHC which is
not the one of your `resolver:`. Also, you should tell Stack where to
find the new libraries and headers. This is especially necessary on OS
X. The special variable `STACK_IN_NIX_EXTRA_ARGS` will be looked for
by `nix-shell` when running the inner `stack` process.
`--extra-lib-dirs` and `--extra-include-dirs` are regular `stack
build` options. You can repeat these options for each dependency.

Defining manually a `shell.nix` file gives you the possibility to
override some Nix derivations ("packages"), for instance to change
some build options of the libraries you use.

And now for the `stack.yaml` file:

```yaml
nix:
  enable: true
  shell-file: shell.nix
```

The `stack build` command will behave exactly the same as above. Note
that specifying both `packages:` and a `shell-file:` results in an
error. (Comment one out before adding the other.)
