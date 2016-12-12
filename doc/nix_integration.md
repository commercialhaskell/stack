# Nix integration

(since 0.1.10.0)

When using the Nix integration, Haskell dependencies are handled as usual: They
are downloaded from Stackage and built locally by Stack. Nix is used by Stack to
provide the _non-Haskell_ dependencies needed by these Haskell packages.

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

### Checking Nix installation

Follow the instructions on the
[Nix download page](http://nixos.org/nix/download.html) to install Nix.  After
doing so, when opening a terminal, the nix commands (`nix-build`, `nix-shell`,
etc) should be available.  If they are not, it should be because the file
located at `$HOME/.nix-profile/etc/profile.d/nix.sh` is not sourced by your shell.

You should either run `source ~/.nix-profile/etc/profile.d/nix.sh` manually
everytime you open a terminal and need Nix or add this command to your
`~/.bashrc` or `~/.bash_profile`.

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

Note that in this mode `stack` can use only GHC versions than have
already been mirrored into the Nix package repository.
The [Nixpkgs master branch](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/haskell-modules)
usually picks up new versions quickly, but it takes two or three
days before those updates arrive in the `unstable` channel. Release
channels, like `nixos-15.09`, receive those updates only
occasionally -- say, every two or three months --, so you should not
expect them to have the latest compiler available. Fresh NixOS installs
use a release version by default.

To know for sure whether a given compiler is available on your system,
you can use the command

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
nix-repl> haskell.compiler.ghc<Tab>
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

If `enable:` is omitted or set to `false`, you can still build in a nix-shell by
passing the `--nix` flag to stack, for instance `stack --nix build`.  Passing
any `--nix*` option to the command line will do the same.

**Known limitation on macOS:** currently, `stack --nix ghci` fails on
macOS, due to a bug in GHCi when working with external shared
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

**Note:** On macOS shells are non-pure by default currently. This is
due soon to be resolved locale issues. So on macOS you'll need to be
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

  # false by default. Must be present and set to `true` to enable Nix.
  # You can set set it in your `$HOME/.stack/config.yaml` to enable
  # Nix for all your projects without having to repeat it
  # enable: true

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

## Using a custom shell.nix file

Nix is also a programming language, and as specified
[here](#nix-integration) if you know it you can provide to the shell
a fully customized derivation as an environment to use. Here is the
equivalent of the configuration used in
[this section](#additions-to-your-stackyaml), but with an explicit
`shell.nix` file (make sure you're using a nixpkgs version later than
2015-03-05):

```nix
{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ glpk pcre ];
}
```

Defining manually a `shell.nix` file gives you the possibility to override some
Nix derivations ("packages"), for instance to change some build options of the
libraries you use, or to set additional environment variables. See the
[Nix manual][nix-manual-exprs] for more. The `buildStackProject` utility
function is documented in the [Nixpkgs manual][nixpkgs-manual-haskell].  In such
case, stack expect this file to define a function of exactly one argument that
should be called `ghc` (as arguments within a set are non-positional), which you
should give to `buildStackProject`. This is the ghc from the resolver you set in
the `stack.yaml`.

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
