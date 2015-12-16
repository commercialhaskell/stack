# Nix integration

(since 0.1.10.0)

`stack` can build automatically inside a nix-shell (the equivalent of
a "container" in Docker parlance), provided Nix is already installed
on your system. To do so, please visit the
[Nix download page](http://nixos.org/nix/download.html).

There are two ways to create a nix-shell:

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

This will instruct `stack` to build inside a nix-shell that will have
the `glpk` and `pcre` libraries installed and available. Further, the
nix-shell will implicitly also include a version of GHC matching the
configured resolver. Enabling Nix support means packages will always
be built using a GHC available inside the shell, rather than your
globally installed one if any.

Note also that this also means that you cannot set your `resolver:` to
something that has not yet been mirrored in the Nixpkgs package
repository. In order to check this, the quickest way is to install and
launch a `nix-repl`:

```sh
$ nix-channel --update
$ nix-env -i nix-repl
$ nix-repl
```

Then, inside the `nix-repl`, do:

```sh
nix-repl> :l <nixpkgs>
nix-repl> haskell.packages.lts-3_13.ghc
```

Replace the resolver version with whatever version you are using. If it outputs
a path of a derivation in the Nix store, like

`«derivation /nix/store/00xx8y0p3r0dqyq2frq277yr1ldqzzg0-ghc-7.10.2.drv»`

then it means that this resolver has been mirrored and exists in your local copy of the nixpkgs. Whereas an error like

`error: attribute ‘lts-3_99’ missing, at (string):1:1`

means you should use a different resolver. You can also use
autocompletion with TAB to know which attributes `haskell.packages`
contains.

In Nixpkgs master branch, you can find the mirrored resolvers in the
Haskell modules
[here on Github](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/haskell-modules).

*Note:* currently, stack only discovers dynamic and static libraries
in the `lib/` folder of any nix package, and likewise header files in
the `include/` folder. If you're dealing with a package that doesn't
follow this standard layout, you'll have to deal with that using
a custom shell file (see below).

### Use stack as normal

With Nix enabled, `stack build` and `stack exec` will automatically
launch themselves in a nix-shell. Note that for now `stack ghci` is bound to
fail on OSX, due to a bug in GHCi when working with external shared
libraries.

If `enable:` is set to `false`, you can still build in a nix-shell by
passing the `--nix` flag to stack, for instance `stack --nix build`.
Passing any `--nix*` option to the command line will do the same.

### The Nix shell

By default, stack will run the build in a pure Nix shell, which means the build should
fail if you haven't specified all the dependencies in the `packages:` section of the
`stack.yaml` file, even if these dependencies are installed elsewhere on your system.
This behaviour enforces complete description of the build environment to facilitate
reproducibility.
To override this behaviour, add `pure: false` to your `stack.yaml` or pass the `--no-nix-pure`
option to the command line.

NOTE: Currently on OSX non pure shells are used by default. This is due to locale
problems that will be sorted out in the future. So do not count on nix shells not being pure
on OSX.

### Package sources

By default, the Nix-shell will look for the nixpkgs directory set by your `NIX_PATH` environment variable.
This will usually be `$HOME/.nix-defexpr/channels/nixpkgs`.

You can override this by passing `--nix-path="nixpkgs=/my/own/nixpkgs/clone"` to ask nix to use
a nixpkgs that you cloned yourself. This e.g. can allow you to use bleeding edge nixpkgs, cloned from [nixpkgs](http://www.github.com/NixOS/nixpkgs) `master` branch, or to rewrite the nix expressions of some packages.
Setting `path: [nixpkgs=/my/own/nixpkgs/clone]` in your `stack.yaml` will do the same.

## Command-line options

The configuration present in your `stack.yaml` can be overriden on the
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

  # true by default. Tells Nix whether to run in a pure shell or not
  pure: true

  # Empty by default. The list of packages you want to be
  # available in the nix-shell at build time (with `stack
  # build`) and run time (with `stack exec`).
  packages: []

  # Unset by default. You cannot set this option if `packages:`
  # is already present and not empty, this will result in an
  # exception
  shell-file: shell.nix

  # A list of strings, empty by default. Additional options that
  # will be passed verbatim to the `nix-shell` command.
  nix-shell-options: []

  # A list of strings, empty by default, such as `[nixpkgs=/my/local/nixpkgs/clone]`
  # that will be used to override the NIX_PATH
  path: []
```
## Using a custom shell.nix file

Nix is also a programming language, and as specified
[here](#nix-integration) if you know it you can provide to the
shell a fully customized derivation as an environment to use. Here is
the equivalent of the configuration used in
[this section](#additions-to-your-stackyaml), but with an explicit `shell.nix`
file:

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
not the one of your `resolver:`. Also, you need to tell Stack where to
find the new libraries and headers. This is especially necessary on OS
X. The special variable `STACK_IN_NIX_EXTRA_ARGS` will be looked for
by the nix-shell when running the inner `stack` process.
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
