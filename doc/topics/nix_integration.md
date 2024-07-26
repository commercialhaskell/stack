<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Nix integration

[:octicons-tag-24: 0.1.10.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.10.0)

[Nix](https://nixos.org/) is a purely functional package manager. Stack can be
configured to integrate with Nix. Integration provides these benefits:

* more reproducible builds. This is because fixed versions of any system
  libraries and commands required to build the project are automatically built
  using Nix and managed locally for each project. These system packages never
  conflict with any existing versions of these libraries on your system. That
  they are managed locally to the project means that you don't need to alter
  your system in any way to build any odd project pulled from the Internet; and
* implicit sharing of system packages between projects. This means you don't
  have more copies on-disk than you need.

The Nix package manager is a pre-requisite for integration. On Linux (including
Windows Subsystem for Linux) and macOS, it can be downloaded and installed from
the [Nix download page](https://nixos.org/download.html).

When integrated with Nix, Stack handles Haskell dependencies as it usually does
and the Nix package manager handles the _non-Haskell_ dependencies needed by the
Haskell packages.

Stack downloads Haskell packages from [Stackage](https://www.stackage.org/lts)
and builds them locally. Stack uses Nix to download
[Nix packages][nix-search-packages]. These provide the GHC compiler and external
C libraries that you would normally install manually.

Nix's `nix-shell` starts an interactive shell based on a Nix expression. Stack
can automatically create a Nix build environment in the background using
`nix-shell`. There are two alternative options to create such a build
environment:

1. provide a list of [Nix packages][nix-search-packages]
2. provide a `shell.nix` file that gives you more control over the libraries and
   tools available inside the shell.

A `shell.nix` file requires writing code in Nix's
[custom language][nix-language]. Use this option only if you know Nix and have
special requirements, such as using custom Nix packages that override the
standard ones or using system libraries with special requirements.

### Checking the Nix installation

Once Nix is installed, the Nix commands (`nix-shell` etc) should be available.
If they are not, it could be because the file
`$HOME/.nix-profile/etc/profile.d/nix.sh` is not sourced by your shell.

You should either:

1. run `source ~/.nix-profile/etc/profile.d/nix.sh` each time you open a
   terminal and need Nix; or
2. add the command `source ~/.nix-profile/etc/profile.d/nix.sh` to your
   `~/.bashrc` or `~/.bash_profile` file.

A Nix path can be specified between angle brackets, e.g. `<nixpkgs>`, and the
directories listed in the `NIX_PATH` environment variable will be searched for
the given file or directory name. Stack makes use of path `<nixpkgs>`. From
Nix 2.4, `NIX_PATH` is not set by `nix.sh`. If `NIX_PATH` is not set, Nix will
fall back to (first) `$HOME/.nix-defexpr/channels` in impure and unrestricted
evaluation mode. However, Stack may use a pure Nix mode (see further
[below](#pure-and-impure-nix-shells)). That directory can be appended to
`NIX_PATH` with
`export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels`. For
information about how Stack itself can configure `NIX_PATH`, see further
[below](#nix-package-sources).

### Enable Nix integration

On NixOS, Nix integration is enabled by default; on other operating systems it
is disabled. To enable Nix integration, add the following section to your Stack
configuration file (`stack.yaml` or `config.yaml`):

~~~yaml
nix:
  enable: true  # false by default, except on NixOS
~~~

The equivalent command line flag (which will prevail) is `--[no-]nix`. Passing
any `--nix-*` option on the command line will imply the `--nix` option.

If Nix integration is not enabled, Stack will notify the user if a `nix`
executable is on the PATH. If that notification is unwanted, it can be muted by
setting Stack's configuration option
[`notify-if-nix-on-path`](../configure/yaml/non-project.md#notify-if-nix-on-path)
to `false`.

With Nix integration enabled, `stack build` and `stack exec` will automatically
launch themselves in a local build environment (using `nix-shell` behind the
scenes). It is not necessary to run `stack setup`, unless you want to cache a
GHC installation before running a build.

**Known limitation on macOS:** currently, `stack --nix ghci` fails on macOS, due
to a bug in GHCi when working with external shared libraries.

### Supporting both Nix and non-Nix developers

With Nix integration enabled in Stack's configuration file, every developer of
your project needs to have Nix installed, but the developer also gets all
external libraries automatically.

Julien Debon of Tweag has published a [blog post][tweag-blog-post] on
*Smooth, non-invasive Haskell Stack and Nix shell integration* (2 June 2022).
The post explains how to set things up so that both Nix and non-Nix developers
can work together on the same project. The `tweag/haskell-stack-nix-example`
[GitHub repository][tweag-example] provides an example of working Stack and Nix
shell integration to accompany the post.

Nix 2.4 (released 1 November 2021) introduced a new and experimental format to
package Nix-based projects, known as 'flakes'.

The example below adapts and extends the example accompanying the blog post
above to use Nix flakes. The `flake.nix` file is:

~~~nix
{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs =
          pkgs.haskell.packages."ghc8107"; # need to match Stackage LTS version
                                           # from stack.yaml snapshot

        myDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}
~~~

Check-in this `flake.nix` to your project's repository. Run the `nix develop`
command (it searches for `flake.nix` by default) and you'll find a new
`flake.lock` file. That file that pins the precise nixpkgs package set. Check-in
that `flake.lock` file as well, and every Nix developer of your project will use
precisely the same package set.

### GHC through Nix packages

Nix integration will instruct Stack to build inside a local build environment.
That environment will also download and use a
[GHC Nix package](https://search.nixos.org/packages?query=haskell.compiler.ghc)
matching the required version of the configured Stack
[snapshot](../configure/yaml/project.md#snapshot).

Enabling Nix integration means that packages will always be built using the
local GHC from Nix inside your shell, rather than your globally installed system
GHC (if any).

Stack can use only GHC versions that are in the Nix package repository. The
[Nixpkgs master branch](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/haskell-modules)
usually picks up new versions quickly, but it takes two or three days before
those updates arrive in the `unstable` channel. Release channels, like
`nixos-22.05`, receive those updates only occasionally -- say, every two or
three months --, so you should not expect them to have the latest compiler
available. Fresh NixOS installs use a release version by default.

To identify whether a given compiler is available, you can use the following Nix
command:

~~~sh
nix-env -f "<nixpkgs>" -qaP -A haskell.compiler.ghc924
haskell.compiler.ghc924  ghc-9.2.4
~~~

If Nix doesn't know that version of GHC, you'll see the following error message:

~~~sh
nix-env -f "<nixpkgs>" -qaP -A haskell.compiler.ghc999
error: attribute ‘ghc999’ in selection path ‘haskell.compiler.ghc999’ not found
~~~

You can list all known Haskell compilers in Nix with the following:

~~~sh
nix-instantiate --eval -E "with import <nixpkgs> {}; lib.attrNames haskell.compiler"
~~~

Alternatively, use `nix repl`, a convenient tool to explore nixpkgs:

~~~sh
nix repl
~~~

In the REPL, load nixpkgs and get the same information through autocomplete:

~~~sh
nix-repl> :l <nixpkgs>
nix-repl> haskell.compiler.ghc<Tab>
~~~

You can type and evaluate any Nix expression in the Nix REPL, such as the one we
gave to `nix-instantiate` earlier.

### External C libraries through Nix packages

To let Nix manage external C libraries, add (for example) the following section
to your Stack configuration file:

~~~yaml
nix:
  enable: true
  packages: [zlib, glpk, pcre]
~~~

The equivalent command line option is `--nix-packages "zlib glpk pcre"`.

The `packages` key and the `shell-file` key (see further below) are
alternatives. Specifying both results in an error.

The example above will instruct Stack to build inside a local build environment
that will have the Nix packages
[zlib](https://search.nixos.org/packages?query=zlib),
[glpk](https://search.nixos.org/packages?query=glpk) and
[pcre](https://search.nixos.org/packages?query=pcre)
installed, which provide the C libraries of the same names.

**Note:** currently, Stack only discovers dynamic and static libraries in the
`lib/` folder of any Nix package, and likewise header files in the `include/`
folder. If you're dealing with a package that doesn't follow this standard
layout, you'll have to deal with that using a custom `shell.nix` file (see
further below).

### External C libraries through a `shell.nix` file

In Nix, a 'derivation' is a description of a build action and its result is a
Nix store object. Nix's [custom language][nix-language] can provide a fully
customized derivation as an environment to use. To specify such a `shell.nix`
file, add the following section to your Stack configuration file:

~~~yaml
nix:
  enable: true
  shell-file: shell.nix
~~~

The equivalent command line option (which will prevail) is
`--nix-shell-file shell.nix`.

The `packages` and `shell-file` keys are alternatives. Specifying both results
in an error.

Defining a `shell.nix` file allow you to override some Nix derivations, for
instance to change some build options of the libraries you use, or to set
additional environment variables. For further information, see the
[Nix manual][nix-manual-exprs].

The `shell.nix` file that is the equivalent of the
`packages: [zlib, glpk, pcre]` example above is:

~~~nix
{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ zlib glpk pcre ];
}
~~~

The `buildStackProject` utility function is documented in the
[Nixpkgs manual][nixpkgs-manual-haskell].

Stack expects the `shell.nix` file to define a function of with one argument
called `ghc` (arguments are not positional), which you should give to
function `buildStackProject`. This argument is a GHC Nix package in the
version as defined in the snapshot you set in Stack's project-level
configuration file (`stack.yaml`, by default).

### Pure and impure Nix shells

By default, Stack will run the build in a *pure* Nix build environment (or
*shell*), which means two important things:

1. basically **no environment variable will be forwarded** from your user
   session to the nix-shell (variables like `HTTP_PROXY` or `PATH` notably will
   not be available); and
2. the build should fail if you haven't specified all the dependencies in the
   `packages:` section of the Stack configuration file, even if these
   dependencies are installed elsewhere on your system. This behaviour enforces
   a complete description of the build environment to facilitate
   reproducibility.

To override this behaviour, add the following section to your Stack YAML
configuration file:

~~~yaml
nix:
  enable: true
  pure: false
~~~

The equivalent command line flag (which will prevail) is `--[no-]-nix-pure`.

**Note:** On macOS, shells are non-pure by default currently. This is due soon
to be resolved locale issues. So on macOS you'll need to be a bit more careful
to check that you really have listed all dependencies.

### Nix package sources

Nix organizes its packages in snapshots of packages (each snapshot being a
"package set") similar to how Stackage organizes Haskell packages.  By default,
`nix-shell` will look for the "nixpkgs" package set located by your `NIX_PATH`
environment variable. This package set can be different depending on when you
installed Nix and which nixpkgs channel you're using (similar to the LTS channel
for stable packages and the nightly channel for bleeding edge packages in
[Stackage](https://www.stackage.org/)). This is bad for reproducibility so that
nixpkgs should be pinned, i.e., set to the same package set for every developer
of your project.

To set or override the Nix package set, add the following section to your Stack
configuration file:

~~~yaml
nix:
  path: [nixpkgs=<path_to_my_own_nixpkgs_clone>]
~~~

The equivalent command line option is
`--nix-path <path_to_my_own_nixpkgs_clone>`.

By this means, you can ask Nix to use your own local checkout of the nixpkgs
repository. You could in this way use a bleeding edge nixpkgs, cloned from the
`NixOS/nixpkgs` [repository](http://www.github.com/NixOS/nixpkgs) `master`
branch, or edit the Nix descriptions of some packages.

The Tweag example [repository][tweag-example] shows how you can pin a package
set.

## Non-project specific configuration

Below is a summary of the non-project specific configuration options and their
default values. The options can be set in Stack's project-level configuration
file (`stack.yaml`, by default) or its global configuration file
(`config.yaml`).

~~~yaml
nix:

  # false by default, except on NixOS. Is Nix integration enabled?
  enable: true

  # true by default. Should Nix run in a pure shell?
  pure: true

  # Empty by default. The list of packages you want to be available in the
  # nix-shell at build time (with `stack build`) and run time (with
  # `stack exec`).
  packages: []

  # Unset by default. You cannot set this option if `packages:`
  # is already present and not empty.
  shell-file: shell.nix

  # A list of strings, empty by default. Additional options that will be passed
  # verbatim to the `nix-shell` command.
  nix-shell-options: []

  # A list of strings, empty by default, such as
  # `[nixpkgs=/my/local/nixpkgs/clone]` that will be used to override
  # NIX_PATH.
  path: []

  # false by default. Whether to add your Nix dependencies as Nix garbage
  # collection roots. This way, calling nix-collect-garbage will not remove
  # those packages from the Nix store, saving you some time when running
  # stack build again with Nix support activated.
  #
  # This creates a `nix-gc-symlinks` directory in the project `.stack-work`.
  # To revert that, just delete this `nix-gc-symlinks` directory.
  add-gc-roots: false
~~~

`stack --nix-help` will list the equivalent command line flags and options.

## Stack and developer tools on NixOS

NixOS is a Linux distribution based on Nix, that is composed using modules and
packages defined in the Nixpkgs project.

When using Stack on NixOS, you must use Stack's Nix integration to install GHC.
That is because external C libraries in NixOS are not installed in the usual
distribution directories. GHC installed through Stack (without Nix) can't find
those libraries and, therefore, can't build most projects. However, GHC provided
through Nix can be modified to find the external C libraries provided through
Nix.

[nix-language]: https://wiki.nixos.org/wiki/Overview_of_the_Nix_Language
[nix-manual-exprs]: http://nixos.org/manual/nix/stable/expressions/writing-nix-expressions.html
[nix-search-packages]: https://search.nixos.org/packages
[nixpkgs-manual-haskell]: https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html?highlight=buildStackProject#how-to-build-a-haskell-project-using-stack
[tweag-blog-post]: https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
[tweag-example]: https://github.com/tweag/haskell-stack-nix-example/
