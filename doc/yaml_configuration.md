# YAML Configuration

This page is intended to fully document all configuration options available in the stack.yaml file. Note that this page is likely to be both *incomplete* and sometimes *inaccurate*. If you see such cases, please update the page, and if you're not sure how, open an issue labeled "question".

The stack.yaml configuration options break down into [project specific](#project-config) options in:

- `<project dir>/stack.yaml`

and [non-project specific](#non-project-config) options in:

- `/etc/stack/config.yaml` -- for system global non-project default options
-  `~/.stack/config.yaml` -- for user non-project default options
- The project file itself may also contain non-project specific options

*Note:* When stack is invoked outside a stack project it will source project specific options from `~/.stack/global/stack.yaml`.  Options in this file will be ignored for a project with its own `<project dir>/stack.yaml`.

## Project config

Project specific options are only valid in the `stack.yaml` file local to a project, not in the user or global config files.

### packages

This lists all local packages. In the simplest usage, it will be a list of directories, e.g.:

```yaml
packages:
- dir1
- dir2
- dir3
```

When the `packages` field is not present, it defaults to looking for a package in the project's root directory:

```yaml
packages:
  - .
```

However, it supports three other location types: an HTTP URL referring to a tarball or a zip that can be downloaded, and information on a Git or Mercurial (since 0.1.10.0) repo to clone, together with this SHA1 commit. For example:

```yaml
packages:
- some-directory
- https://example.com/foo/bar/baz-0.0.2.tar.gz
- location:
    git: git@github.com:commercialhaskell/stack.git
    commit: 6a86ee32e5b869a877151f74064572225e1a0398
- location:
    hg: https://example.com/hg/repo
    commit: da39a3ee5e6b4b0d3255bfef95601890afd80709
```

Note: it is highly recommended that you only use SHA1 values for a Git or Mercurial commit. Other values may work, but they are not officially supported, and may result in unexpected behavior (namely, stack will not automatically pull to update to new versions).

stack further allows you to tweak your packages by specifying two additional
settings:

* A list of subdirectories to build (useful for mega-repos like
[wai](https://github.com/yesodweb/wai/) or
[digestive-functors](https://github.com/jaspervdj/digestive-functors))
* Whether a package should be treated as a dependency: a package marked `extra-dep: true` will only be built if demanded by a non-dependency, and its test suites and benchmarks will not be run. This is useful for tweaking upstream packages.

To tie this all together, here's an example of the different settings:

```yaml
packages:
- local-package
- location: vendor/binary
  extra-dep: true
- location:
    git: git@github.com:yesodweb/wai
    commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
  subdirs:
  - auto-update
  - wai
```

Instead of using Git to clone from Github, it is also possible to use the 'Download commit as Zip' feature of the website. For example:

```yaml
packages:
- location: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
  subdirs:
  - auto-update
  - wai
```

### extra-deps

This is a list of package identifiers for additional packages from upstream to
be included. This is usually used to augment an LTS Haskell or Stackage Nightly
snapshot with a package that is not present or is at an different version than you
wish to use.

```yaml
extra-deps:
- acme-missiles-0.3
```

### resolver

Specifies how dependencies are resolved. There are currently four resolver types:

* LTS Haskell snapshots, e.g. `resolver: lts-2.14`
* Stackage Nightly snapshot, e.g. `resolver: nightly-2015-06-16`
* No snapshot, just use packages shipped with the compiler
    * For GHC this looks like `resolver: ghc-7.10.2`
    * For GHCJS this looks like `resolver: ghcjs-0.1.0_ghc-7.10.2`.
* [Custom snapshot](https://github.com/commercialhaskell/stack/wiki/Custom-Snapshot)

Each of these resolvers will also determine what constraints are placed on the compiler version. See the [compiler-check](#compiler-check) option for some additional control over compiler version.

### flags

Flags can be set for each package separately, e.g.

```yaml
flags:
  package-name:
    flag-name: true
```

Flags will only affect packages in your `packages` and `extra-deps` settings.
Packages that come from the snapshot global database or are not affected.

### image

The image settings are used for the creation of container images using `stack
image container`, e.g.

```yaml
image:
  containers:
    - base: "fpco/stack-build"
      add:
        static: /data/static
```

`base` is the docker image that will be used to built upon. The `add` lines
allow you to add additional directories to your image. You can specify the name
of the image using `name` (otherwise it defaults to the same as your project).
You can also specify `entrypoints`. By default all your executables are placed
in `/usr/local/bin`, but you can specify a list using `executables` to only add
some.

### user-message

A user-message is inserted by `stack init` when it omits packages or adds
external dependencies. For example:

```yaml
user-message: ! 'Warning: Some packages were found to be incompatible with the resolver
  and have been left commented out in the packages section.

  Warning: Specified resolver could not satisfy all dependencies. Some external packages
  have been added as dependencies.

  You can suppress this message by removing it from stack.yaml

'
```

This messages is displayed every time the config is loaded by stack and serves
as a reminder for the user to review the configuration and make any changes if
needed. The user can delete this message if the generated configuration is
acceptable.

## Non-project config

Non-project config options may go in the global config (`/etc/stack/config.yaml`) or the user config (`~/.stack/config.yaml`).

### docker

See [Docker integration](docker_integration.html#configuration).

### nix

(since 0.1.10.0)

See [Nix integration](nix_integration.html#configuration).

### connection-count

Integer indicating how many simultaneous downloads are allowed to happen

Default: `8`

### hide-th-loading

Strip out the "Loading ..." lines from GHC build output, produced when using Template Haskell

Default: `true`

### latest-snapshot-url

URL providing a JSON with information on the latest LTS and Nightly snapshots, used for automatic project configuration.

Default: `https://www.stackage.org/download/snapshots.json`

### local-bin-path

Target directory for `stack install` and `stack build --copy-bins`.

Default: `~/.local/bin`

### package-indices

```yaml
package-indices:
- name: Hackage
  download-prefix: https://s3.amazonaws.com/hackage.fpcomplete.com/package/

  # at least one of the following must be present
  git: https://github.com/commercialhaskell/all-cabal-hashes.git
  http: https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz

  # optional fields, both default to false
  gpg-verify: false
  require-hashes: false
```

One thing you should be aware of: if you change the contents of package-version
combination by setting a different package index, this *can* have an effect on
other projects by installing into your shared snapshot database.

### system-ghc

Enables or disables using the GHC available on the PATH. Useful to disable if
you want to force stack to use its own installed GHC (via `stack setup`), in
cases where your system GHC my be incomplete for some reason. Default is `true`.

```yaml
# Turn off system GHC
system-ghc: false
```

### install-ghc

Whether or not to automatically install GHC when necessary. Default is `false`,
which means stack will prompt you to run `stack setup` as needed.

### skip-ghc-check

Should we skip the check to confirm that your system GHC version (on the PATH) matches what your project expects? Default is `false`.

### require-stack-version

Require a version of stack within the specified range
([cabal-style](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information))
to be used for this project. Example: `require-stack-version: "== 0.1.*"`

Default: `"-any"`

### arch/os

Set the architecture and operating system for GHC, build directories, etc. Values are those recognized by Cabal, e.g.:

    arch: i386, x86_64
    os: windows, linux

You likely only ever want to change the arch value. This can also be set via the command line.

### extra-include-dirs/extra-lib-dirs

A list of extra paths to be searched for header files and libraries, respectively. Paths should be absolute

```yaml
extra-include-dirs:
- /opt/foo/include
extra-lib-dirs:
- /opt/foo/lib
```

### compiler-check

(Since 0.1.4)

Specifies how the compiler version in the resolver is matched against concrete versions. Valid values:

* `match-minor`: make sure that the first three components match, but allow patch-level differences. For example< 7.8.4.1 and 7.8.4.2 would both match 7.8.4. This is useful to allow for custom patch levels of a compiler. This is the default
* `match-exact`: the entire version number must match precisely
* `newer-minor`: the third component can be increased, e.g. if your resolver is `ghc-7.10.1`, then 7.10.2 will also be allowed. This was the default up through stack 0.1.3

### compiler

(Since 0.1.7)

Overrides the compiler version in the resolver. Note that the `compiler-check`
flag also applies to the version numbers. This uses the same syntax as compiler
resolvers like `ghc-7.10.2` or `ghcjs-0.1.0.20150924_ghc-7.10.2` (version used
for the 'old-base' version of GHCJS).  While it's useful to override the
compiler for a variety of reasons, the main usecase is to use GHCJS with a
stackage snapshot, like this:

```yaml
resolver: lts-3.10
compiler: ghcjs-0.1.0.20150924_ghc-7.10.2
compiler-check: match-exact
```

### ghc-options

(Since 0.1.4)

Allows specifying per-package and global GHC options:

```yaml
ghc-options:
    # All packages
    "*": -Wall
    some-package: -DSOME_CPP_FLAG
```

Caveat emptor: setting options like this will affect your snapshot packages,
which can lead to unpredictable behavior versus official Stackage snapshots.
This is in contrast to the `ghc-options` command line flag, which will only
affect local packages.

### ghc-variant

(Since 0.1.5)

Specify a variant binary distribution of GHC to use.  Known values:

* `standard`: This is the default, uses the standard GHC binary distribution
* `gmp4`: Use the "centos6" GHC bindist, for Linux systems with libgmp4 (aka
  `libgmp.so.3`), such as CentOS 6. This variant will be used automatically on such systems; you should not need to specify it in the configuration
* `integersimple`: Use a GHC bindist that uses
  [integer-simple instead of GMP](https://ghc.haskell.org/trac/ghc/wiki/ReplacingGMPNotes)
* any other value: Use a custom GHC bindist. You should specify
  [setup-info](#setup-info) so `stack setup` knows where to download it, or
  pass the `stack setup --ghc-bindist` argument on the command-line

### setup-info

(Since 0.1.5)

Allows overriding from where tools like GHC and msys2 (on Windows) are
downloaded. Most useful for specifying locations of custom GHC binary
distributions (for use with the [ghc-variant](#ghc-variant) option):

```yaml
setup-info:
  ghc:
    windows32-custom-foo:
      7.10.2:
        url: "https://example.com/ghc-7.10.2-i386-unknown-mingw32-foo.tar.xz"
```

### pvp-bounds

(Since 0.1.5)

When using the `sdist` and `upload` commands, this setting determines whether
the cabal file's dependencies should be modified to reflect PVP lower and upper
bounds. Values are `none` (unchanged), `upper` (add upper bounds), `lower` (add
lower bounds), and both (and upper and lower bounds). The algorithm it follows
is:

* If an upper or lower bound already exists on a dependency, it's left alone
* When adding a lower bound, we look at the current version specified by stack.yaml, and set it as the lower bound (e.g., `foo >= 1.2.3`)
* When adding an upper bound, we require less than the next major version (e.g., `foo < 1.3`)

```yaml
pvp-bounds: none
```

For more information, see [the announcement blog post](https://www.fpcomplete.com/blog/2015/09/stack-pvp).

### modify-code-page

(Since 0.1.6)

Modify the code page for UTF-8 output when running on Windows. Default behavior
is to modify.

```yaml
modify-code-page: false
```

### explicit-setup-deps

(Since 0.1.6)

Decide whether a custom `Setup.hs` script should be run with an explicit list of
dependencies, based on the dependencies of the package itself. It associates the
name of a local package with a boolean. When it's `true`, the `Setup.hs` script
is built with an explicit list of packages. When it's `false` (default), the
`Setup.hs` script is built without access to the local DB, but can access any
package in the snapshot / global DB.

Note that in the future, this will be unnecessary, once Cabal provides full
support for explicit Setup.hs dependencies.

```yaml
explicit-setup-deps:
    "*": true # change the default
    entropy: false # override the new default for one package
```

### rebuild-ghc-options

(Since 0.1.6)

Should we rebuild a package when its GHC options change? Before 0.1.6, this was a non-configurable true. However, in most cases, the flag is used to affect optimization levels and warning behavior, for which GHC itself doesn't actually recompile the modules anyway. Therefore, the new behavior is to not recompile on an options change, but this behavior can be changed back with the following:

```yaml
rebuild-ghc-options: true
```

### apply-ghc-options

(Since 0.1.6)

Which packages do ghc-options on the command line get applied to? Before 0.1.6, the default value was `targets`

```yaml
apply-ghc-options: locals # all local packages, the default
# apply-ghc-options: targets # all local packages that are targets
# apply-ghc-options: everything # applied even to snapshot and extra-deps
```

Note that `everything` is a slightly dangerous value, as it can break invariants about your snapshot database.

### allow-newer

(Since 0.1.7)

Ignore version bounds in .cabal files. Default is false.

```yaml
allow-newer: true
```

Note that this also ignores lower bounds. The name "allow-newer" is chosen to
match the commonly used cabal option.

### templates

Templates used with `stack new` have a number of parameters that affect the generated code. These can be set for all new projects you create. The result of them can be observed in the generated LICENSE and cabal files.

The 5 parameters are: `author-email`, `author-name`, `category`, `copyright` and `github-username`.

* _author-email_ - sets the `maintainer` property in cabal
* _author-name_ - sets the `author` property in cabal and the name used in LICENSE
* _category_ - sets the `category` property in cabal. This is used in Hackage. For examples of categories see [Packages by category](https://hackage.haskell.org/packages/). It makes sense for `category` to be set on a per project basis because it is uncommon for all projects a user creates to belong to the same category. The category can be set per project by passing `-p "category:value"` to the `stack new` command.
* _copyright_ - sets the `copyright` property in cabal. It is typically the name of the holder of the copyright on the package and the year(s) from which copyright is claimed. For example: `Copyright: (c) 2006-2007 Joe Bloggs`
* _github-username_ - used to generate `homepage` and `source-repository` in cabal. For instance `github-username: myusername` and `stack new my-project new-template` would result:
```yaml
homepage: http://github.com/myusername/my-project#readme

source-repository head
  type: git
  location: https://github.com/myusername/my-project
```

These properties can be set in `config.yaml` as follows:
```yaml
templates:
  params:
    author-name: Your Name
    author-email: youremail@example.com
    category: Your Projects Category
    copyright: 'Copyright: (c) 2016 Your Name'
    github-username: yourusername
```
