# Using Nix with Stack

`stack` can build automatically inside a Nix-shell, provided Nix is already installed on your system.
There are two ways of doing that:

- providing a list of packages (by attribute name) existing in [Nixpkgs](http://nixos.org/nixos/packages.html), or
- providing a custom `shell.nix` file defining a derivation that will be used to launch the shell.

The second requires a fully explicit configuration. So use this option only if
you already know Nix and have special requirements, as using one or several
overriden Nix derivations or using libraries which are not laid out in standard
way once installed in the Nix store.

## Usage

To install Nix, please visit the [Nix download page](http://nixos.org/nix/download.html).

### Enable in stack.yaml

Add a section to your stack.yaml as follows:

    nix-shell:
        enable: true
        packages: [glpk, pcre]

It will make `stack` build inside a Nix-shell that will first install and make available the `glpk` and `pcre` libraries.

Stack expects every library used this way to provide a `lib` and an `include`
subdirectories directly in the directory where the library is installed in the
nix store, which is the case for most libraries in Nixpkgs.

On both Linux and MacOSX, this will automatically add a dependency to GHC
according to which `resolver:` is used in your `stack.yaml` configuration.  This
means that when using the Nix support, stack no longer builds using a locally
installed GHC, as GHC becomes yet another Nix dependency.

This also means that you cannot set your `resolver:` to something that has not yet been mirrored in the Nixpkgs. In order to check this, the quickest way is to install and launch a `nix-repl`:

```
$ nix-channel --update
$ nix-env -i nix-repl
$ nix-repl
```

Then, inside the `nix-repl`, do:

```
nix-repl> :l <nixpkgs>
nix-repl> haskell.packages.lts-3_13.ghc
```

Replace the resolver version with whatever version you are using. If it outputs
a path of a derivation in the Nix store, like

`«derivation /nix/store/00xx8y0p3r0dqyq2frq277yr1ldqzzg0-ghc-7.10.2.drv»`

then it means this resolver has been mirrored. Whereas an error like

`error: attribute ‘lts-3_14’ missing, at (string):1:1`

means you should use a different resolver.
You can also use autocompletion with TAB to know which attributes `haskell.packages` contains.

In Nixpkgs master branch, you can find the mirrored resolvers in the Haskell modules [here on Github](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/haskell-modules).

### Use stack as normal

With Nix enabled, `stack build` and `stack exec` will automatically launch themselves in a nix-shell. Note that for now `stack ghci` will not work, due to a bug in GHCi when working with external shared libraries.

Note that if `enable:` is set to `false`, you can still build in a nix-shell by passing the `--nix` flag to stack, for instance `stack --nix build`.

## Command-line options

The configuration present in your `stack.yaml` can be overriden on the command-line. See `stack --nix-help` for a list of all Nix options.


## Configuration

`stack.yaml` contains a `nix-shell:` section with Nix settings. Without this section, Nix will not be used.

Here is a commented configuration file, showing the default values:

    nix-shell:

        # `true` by default when the nix-shell section is present.
        # Set it to `false` to disable using Nix.
        enable: true

        # Empty by default. The list of packages you want to be available
        # in the nix-shell at build time (with `stack build`) and
        #run time (with `stack exec`).
        packages: []

        # Unset by default. You cannot set this option if `packages:`
        # is already present and not empty, this will result in
        # an exception
        shell-file: shell.nix

        # A list of strings, empty by default.
        # Additional options that will be passed
        # verbatim to the `nix-shell` command.
        nix-shell-options: []

## Using a custom shell.nix file

Nix is also a programming language, and as specified [here](#using-nix-with-stack) if you know it you can provide
to the shell a fully customized derivation as an environment to use.  Here is
the equivalent of the configuration used in [this section](#enable-in-stackyaml), but with an explicit `shell.nix`
file:

```
with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "myEnv";
  buildInputs = [glpk pcre haskell.packages.lts-3_13.ghc];
  STACK_IN_NIX_EXTRA_ARGS="--extra-lib-dirs=${glpk}/lib --extra-include-dirs=${glpk}/include --extra-lib-dirs=${pcre}/lib --extra-include-dirs=${pcre}/include";
}
```

Note that in that case, you _have_ to include (a version of) GHC in your
`buildInputs`! This potentially allows you to use a GHC which is not the one of
your `resolver:`. Also, you need to tell Stack where to find the new libraries
and headers. This is especially necessary on MacOSX. The special variable
`STACK_IN_NIX_EXTRA_ARGS` will be looked for by the nix-shell when running the
inner `stack` process.  `--extra-lib-dirs` and `--extra-include-dirs` are
regular `stack build` options. You can repeat these options for each dependency.

Defining manually a `shell.nix` file gives you the possibility to override some
Nix derivations ("packages"), for instance to change some build options of the
libraries you use.

And now for the `stack.yaml` file:

```
nix-shell:
    enable: true
    shell-file: shell.nix
```

The `stack build` command will behave exactly the same as above. Note that specifying both `packages:`
and a `shell-file:` results in an error. (Comment one out before adding the other.)
