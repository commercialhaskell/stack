<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# YAML Configuration

This page is intended to fully document all configuration options available in
the stack.yaml file. Note that this page is likely to be both *incomplete* and
sometimes *inaccurate*. If you see such cases, please update the page, and if
you're not sure how, open an issue labeled "question".

The stack.yaml configuration options break down into [project-specific](#project-specific-config) options in:

- `<project dir>/stack.yaml`

and [non-project-specific](#non-project-specific-config) options in:

- `/etc/stack/config.yaml` -- for system global non-project default options
-  `~/.stack/config.yaml` -- for user non-project default options
- The project file itself may also contain non-project specific options

*Note:* When stack is invoked outside a stack project it will source project
specific options from `~/.stack/global-project/stack.yaml`. When stack is
invoked inside a stack project, only options from `<project dir>/stack.yaml` are
used, and `~/.stack/global-project/stack.yaml` is ignored.

## Project-specific config

Project-specific options are only valid in the `stack.yaml` file local to a
project, not in the user or global config files.

> Note: We define **project** to mean a directory that contains a `stack.yaml`
> file, which specifies how to build a set of packages. We define **package** to
> be a package with a `.cabal` file or Hpack `package.yaml` file.

In your project-specific options, you specify both **which local packages** to
build and **which dependencies to use** when building these packages. Unlike the
user's local packages, these dependencies aren't built by default. They only get
built when needed.

Shadowing semantics, described
[here](http://docs.haskellstack.org/en/stable/architecture/#shadowing), are
applied to your configuration. So, if you add a package to your `packages` list,
it will be used even if you're using a snapshot that specifies a particular
version. Similarly, `extra-deps` will shadow the version specified in the
resolver.

### resolver

Specifies which snapshot is to be used for this project. A snapshot
defines a GHC version, a number of packages available for
installation, and various settings like build flags. It is called a
resolver since a snapshot states how dependencies are resolved. There
are currently four resolver types:

* LTS Haskell snapshots, e.g. `resolver: lts-2.14`
* Stackage Nightly snapshot, e.g. `resolver: nightly-2015-06-16`
* No snapshot, just use packages shipped with the compiler
    * For GHC this looks like `resolver: ghc-7.10.2`
    * For GHCJS this looks like `resolver: ghcjs-0.1.0_ghc-7.10.2`.
* [Custom snapshot](custom_snapshot.md)

Each of these resolvers will also determine what constraints are placed on the
compiler version. See the [compiler-check](#compiler-check) option for some
additional control over compiler version.

### packages and extra-deps

_NOTE_ The contents of this section have changed significantly since
extensible snapshots were implemented (see:
[writeup](https://www.fpcomplete.com/blog/2017/07/stacks-new-extensible-snapshots)
and
[PR #3249](https://github.com/commercialhaskell/stack/pull/3249)). Most
old syntax is still supported with newer versions of Stack, but will
not be documented here. Instead, this section contains the recommended
syntax as of Stack v1.6.0.

There are two types of packages that can be defined in your
`stack.yaml` file:

* __Project packages__, those which you are actually working on in
  your current project. These are local file paths in your project
  directory.
* __Extra dependencies__, which are packages provided locally on top
  of the snapshot definition of available packages. These can come
  from Hackage (or an alternative package index you've defined, see
  [package-indices](#package-indices)), an HTTP(S) or local archive, a
  Git or Mercurial repository, or a local file path.

These two sets of packages are both installed into your local package
database within your project. However, beyond that, they are
completely different:

* Project packages will be built by default with a `stack build`
  without specific targets. Extra dependencies will only be built if
  they are depended upon.
* Test suites and benchmarks may be run for project packages. They are
  never run for extra dependencies.

The `packages` key is a simple list of file paths, which will be
treated as relative to the directory containing your `stack.yaml`
file. For example:

```yaml
packages:
- .
- dir1/dir2
```

Each package directory or location specified must have a valid cabal
file or hpack `package.yaml` file present. Note that the
subdirectories of the directory are not searched for cabal
files. Subdirectories will have to be specified as independent items
in the list of packages.

When the `packages` field is not present, it defaults to looking for a package
in the project's root directory:

```yaml
packages:
- .
```

The `extra-deps` key is given a list of all extra dependencies. If
omitted, it is taken as the empty list, e.g.:

```yaml
extra-deps: []
```

It supports four different styles of values:

#### Package index

Packages can be stated by a name/version combination, which will be
looked up in the package index (by default, Hackage). The basic syntax
for this is:

```yaml
extra-deps:
- acme-missiles-0.3
```

Using this syntax, the most recent Cabal file revision available will
be used. For more reproducibility of builds, it is recommended to
state the SHA256 hash of the cabal file contents as well, like this:

```yaml
extra-deps:
- acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
```

Or a specific revision number, with `0` being the original file:

```yaml
extra-deps:
- acme-missiles-0.3@rev:0
```

Note that specifying via SHA256 is slightly more resilient in that it
does not rely on correct ordering in the package index, while revision
number is likely simpler to use. In practice, both should guarantee
equally reproducible build plans.

If unspecified, `subdirs` defaults to `['.']` (i.e. look only in the top-level
directory).  Note that if you specify a value of `subdirs`, then `'.'` is _not_
included by default and needs to be explicitly specified if a required package
is found in the top-level directory of the repository.

#### Local file path

Like `packages`, local file paths can be used in `extra-deps`, and
will be relative to the directory containing the `stack.yaml` file.

```yaml
extra-deps:
- vendor/somelib
```

Note that if a local directory can be parsed as a package identifier,
Stack will treat it as a package identifier. In other words, if you
have a local directory named `foo-1.2.3`, instead of:

```yaml
extra-deps:
- foo-1.2.3
```

You should use the following to be explicit:

```yaml
extra-deps:
- ./foo-1.2.3
```

#### Git and Mercurial repos

You can give a Git or Mercurial repo at a specific commit, and Stack
will clone that repo.

```yaml
extra-deps:
- git: git@github.com:commercialhaskell/stack.git
  commit: 6a86ee32e5b869a877151f74064572225e1a0398
- hg: https://example.com/hg/repo
  commit: da39a3ee5e6b4b0d3255bfef95601890afd80709
```

__NOTE__ It is highly recommended that you only use SHA1 values for a
Git or Mercurial commit. Other values may work, but they are not
officially supported, and may result in unexpected behavior (namely,
Stack will not automatically pull to update to new versions).
Another problem with this is that your build will not be deterministic,
because when someone else tries to build the project they can get a
different checkout of the package.

A common practice in the Haskell world is to use "megarepos", or
repositories with multiple packages in various subdirectories. Some
common examples include [wai](https://github.com/yesodweb/wai/) and
[digestive-functors](https://github.com/jaspervdj/digestive-functors). To
support this, you may also specify `subdirs` for repositories, e.g.:

```yaml
extra-deps:
- git: git@github.com:yesodweb/wai
  commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
  subdirs:
  - auto-update
  - wai
```

If unspecified, `subdirs` defaults to `subdirs: [.]`, or looking for a
package in the root of the repo.

#### Archives (HTTP(S) or local filepath)

This one's pretty straightforward: you can use HTTP and HTTPS URLs and
local filepaths referring to either tarballs or ZIP files.

__NOTE__ Stack assumes that these files never change after downloading
to avoid needing to make an HTTP request on each build.

```yaml
extra-deps:
- https://example.com/foo/bar/baz-0.0.2.tar.gz
- archive: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
  subdirs:
  - wai
  - warp
- archive: ../acme-missiles-0.3.tar.gz
  sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b
```

Note that HTTP(S) URLs also support `subdirs` like repos to allow for
archives of megarepos. In order to leverage this, use `location:
http://...`.

### flags

Flags can be set for each package separately, e.g.

```yaml
flags:
  package-name:
    flag-name: true
```

If a specified flag is different than the one specified for a snapshot package,
then the snapshot package will automatically be promoted to be an extra-dep.

### image

The image settings are used for the creation of container images using
`stack image container`, e.g.

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

When you specify `entrypoints`, multiple containers will be built:  a project
container, and one container for each entrypoint.

For example the following configuration:

```yaml
image:
  containers:
  - name: myproject
    base: fpco/stack-run
    add:
      production/app-backend/conf/: /etc/app-backend
    entrypoints:
    - app-backend
```

will build one container tagged `myproject:latest` which contains the project
including the `/etc/app-backend` configuration data.

Another container tagged `myproject-app-backend:latest` based on the `myproject:latest`
will additionally contain the logic for starting the `app-backend` entrypoint.


### user-message

A user-message is inserted by `stack init` when it omits packages or adds
external dependencies. For example:

```yaml
user-message: ! 'Warning: Some packages were found to be incompatible with the resolver
  and have been left commented out in the packages section.

  Warning: Specified resolver could not satisfy all dependencies. Some external packages
  have been added as dependencies.

  You can omit this message by removing it from stack.yaml

'
```

This messages is displayed every time the config is loaded by stack and serves
as a reminder for the user to review the configuration and make any changes if
needed. The user can delete this message if the generated configuration is
acceptable.

## Non-project-specific config

Non-project config options may go in the global config (`/etc/stack/config.yaml`) or the user config (`~/.stack/config.yaml`).

### docker

See [Docker integration](docker_integration.md#configuration).

### nix

(since 0.1.10.0)

See [Nix integration](nix_integration.md#configuration).

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

  # HTTP location of the package index
  http: https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz

  # Or, if using Hackage Security below, give the root URL:
  http: https://s3.amazonaws.com/hackage.fpcomplete.com/

  # optional fields, both default to false
  require-hashes: false

  # Starting with stack 1.4, we default to using Hackage Security
  hackage-security:
    keyids: ["deadbeef", "12345"] # list of all approved keys
    key-threshold: 3 # number of keys required
```

One thing you should be aware of: if you change the contents of package-version
combination by setting a different package index, this *can* have an effect on
other projects by installing into your shared snapshot database.

Note that older versions of Stack supported Git-based indices. This feature has since been removed. A line such as:

```yaml
git: https://github.com/commercialhaskell/all-cabal-hashes.git
gpg-verify: false
```

Will now be ignored.

__IMPORTANT__ Hackage and its mirrors typically have two index files
available: `00-index.tar.gz` and `01-index.tar.gz`. The former is a
legacy file for backwards compatibility. It does not contain the cabal
file revisions produced by Hackage, and therefore _will not work_ with
most snapshots. Instead, you need to use `01-index.tar.gz` to ensure
that exact revisions can be found, ensuring more reproducible builds.

### system-ghc

Enables or disables using the GHC available on the PATH.
Useful to enable if you want to save the time, bandwidth or storage space needed to setup an isolated GHC.
Default is `false` unless the [Docker](docker_integration.md) or [Nix](nix_integration.md) integration is enabled.
In a Nix-enabled configuration, stack is incompatible with `system-ghc: false`.

```yaml
# Turn on system GHC
system-ghc: true
```

### install-ghc

Whether or not to automatically install GHC when necessary. Since
Stack 1.5.0, the default is `true`, which means Stack will not ask you
before downloading and installing GHC.

### skip-ghc-check

Should we skip the check to confirm that your system GHC version (on the PATH)
matches what your project expects? Default is `false`.

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

Since these are system-dependent absolute paths, it is recommended that you
specify these in your `config.yaml` within the stack root (usually, `~/.stack`).
If you control the build environment in your project's ``stack.yaml``, perhaps
through docker or other means, then it may well make sense to include these
there as well.


### with-gcc

Specify a path to gcc explicitly, rather than relying on the normal path resolution.

```yaml
with-gcc: /usr/local/bin/gcc-5
```

### with-hpack

Use an Hpack executable, rather than using the bundled Hpack.

```yaml
with-hpack: /usr/local/bin/hpack
```

### compiler-check

(Since 0.1.4)

Specifies how the compiler version in the resolver is matched against concrete versions. Valid values:

* `match-minor`: make sure that the first three components match, but allow
  patch-level differences. For example< 7.8.4.1 and 7.8.4.2 would both match
  7.8.4. This is useful to allow for custom patch levels of a compiler. This is
  the default
* `match-exact`: the entire version number must match precisely
* `newer-minor`: the third component can be increased, e.g. if your resolver is
  `ghc-7.10.1`, then 7.10.2 will also be allowed. This was the default up
  through stack 0.1.3

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
    "$locals": -Wall
    "$targets": -Werror
    "$everything": -O2
    some-package: -DSOME_CPP_FLAG
```

Since 1.6.0, setting a GHC options for a specific package will
automatically promote it to a local package (much like setting a
custom package flag). However, setting options via `$everything` on all flags
will not do so (see
[Github discussion](https://github.com/commercialhaskell/stack/issues/849#issuecomment-320892095)
for reasoning). This can lead to unpredicable behavior by affecting
your snapshot packages.

The behavior of the `$locals`, `$targets`, and `$everything` special
keys mirrors the behavior for the
[`apply-ghc-options` setting](#apply-ghc-options), which affects
command line parameters.

NOTE: Prior to version 1.6.0, the `$locals`, `$targets`, and
`$everything` keys were not support. Instead, you could use `"*"` for
the behavior represented now by `$everything`. It is highly
recommended to switch to the new, more expressive, keys.

### apply-ghc-options

(Since 0.1.6)

Which packages do ghc-options on the command line get applied to? Before 0.1.6, the default value was `targets`

```yaml
apply-ghc-options: locals # all local packages, the default
# apply-ghc-options: targets # all local packages that are targets
# apply-ghc-options: everything # applied even to snapshot and extra-deps
```

Note that `everything` is a slightly dangerous value, as it can break invariants about your snapshot database.

### rebuild-ghc-options

(Since 0.1.6)

Should we rebuild a package when its GHC options change? Before 0.1.6, this was
a non-configurable true. However, in most cases, the flag is used to affect
optimization levels and warning behavior, for which GHC itself doesn't actually
recompile the modules anyway. Therefore, the new behavior is to not recompile
on an options change, but this behavior can be changed back with the following:

```yaml
rebuild-ghc-options: true
```

### ghc-variant

(Since 0.1.5)

Specify a variant binary distribution of GHC to use.  Known values:

* `standard`: This is the default, uses the standard GHC binary distribution
* `integersimple`: Use a GHC bindist that uses
  [integer-simple instead of GMP](https://ghc.haskell.org/trac/ghc/wiki/ReplacingGMPNotes)
* any other value: Use a custom GHC bindist. You should specify
  [setup-info](#setup-info) so `stack setup` knows where to download it, or
  pass the `stack setup --ghc-bindist` argument on the command-line

This option is incompatible with `system-ghc: true`.

### ghc-build

(Since 1.3.0)

Specify a specialized architecture bindist to use.  Normally this is
determined automatically, but you can override the autodetected value here.
Possible arguments include `standard`, `gmp4`, `tinfo6`, and `nopie`.

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

Or without using `ghc-variant`:

```yaml
setup-info: "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup-2.yaml"
```

`url` may be either URL or (since 1.2.0) absolute file path.

### pvp-bounds

(Since 0.1.5)

__NOTE__ As of Stack 1.6.0, this feature does not reliably work, due
to issues with the Cabal library's printer. Stack will generate a
warning when a lossy conversion occurs, in which case you may need to
disable this setting. See
[#3550](https://github.com/commercialhaskell/stack/issues/3550) for
more information.

When using the `sdist` and `upload` commands, this setting determines whether
the cabal file's dependencies should be modified to reflect PVP lower and upper
bounds. Values are `none` (unchanged), `upper` (add upper bounds), `lower` (add
lower bounds), and both (and upper and lower bounds). The algorithm it follows
is:

* If an upper or lower bound already exists on a dependency, it's left alone
* When adding a lower bound, we look at the current version specified by
  stack.yaml, and set it as the lower bound (e.g., `foo >= 1.2.3`)
* When adding an upper bound, we require less than the next major version
  (e.g., `foo < 1.3`)

```yaml
pvp-bounds: none
```

For more information, see [the announcement blog post](https://www.fpcomplete.com/blog/2015/09/stack-pvp).

__NOTE__ Since Stack 1.5.0, each of the values listed above supports
adding `-revision` to the end of each value, e.g. `pvp-bounds:
both-revision`. This means that, when uploading to Hackage, Stack will
first upload your tarball with an unmodified `.cabal` file, and then
upload a cabal file revision with the PVP bounds added. This can be
useful&mdash;especially combined with the
[Stackage no-revisions feature](http://www.snoyman.com/blog/2017/04/stackages-no-revisions-field)&mdash;as
a method to ensure PVP compliance without having to proactively fix
bounds issues for Stackage maintenance.

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

NOTE: since 1.4.0, Stack has support for Cabal's `custom-setup` block
(introduced in Cabal 1.24). If a `custom-setup` block is provided in a `.cabal`
file, it will override the setting of `explicit-setup-deps`, and instead rely
on the stated dependencies.

### allow-newer

(Since 0.1.7)

Ignore version bounds in .cabal files. Default is false.

```yaml
allow-newer: true
```

Note that this also ignores lower bounds. The name "allow-newer" is chosen to
match the commonly used cabal option.

### allow-different-user

(Since 1.0.1)

Allow users other than the owner of the stack root directory (typically `~/.stack`)
to use the stack installation. The default is `false`. POSIX systems only.

```yaml
allow-different-user: true
```

The intention of this option is to prevent file permission problems, for example
as the result of a `stack` command executed under `sudo`.

The option is automatically enabled when `stack` is re-spawned in a Docker process.

### build

(Since 1.1.0)

Allows setting build options which are usually specified on the CLI.  Here are
the settings with their defaults:

```yaml
build:
  library-profiling: false
  executable-profiling: false
  copy-bins: false
  prefetch: false
  keep-going: false

  # NOTE: global usage of haddock can cause build failures when documentation is
  # incorrectly formatted.  This could also affect scripts which use stack.
  haddock: false
  haddock-arguments:
    haddock-args: []      # Additional arguments passed to haddock, --haddock-arguments
    # haddock-args:
    # - "--css=/home/user/my-css"
  open-haddocks: false    # --open
  haddock-deps: false     # if unspecified, defaults to true if haddock is set
  haddock-internal: false

  # These are inadvisable to use in your global configuration, as they make the
  # stack build CLI behave quite differently.
  test: false
  test-arguments:
    rerun-tests: true   # Rerun successful tests
    additional-args: [] # --test-arguments
    # additional-args:
    # - "--fail-fast"
    coverage: false
    no-run-tests: false
  bench: false
  benchmark-opts:
    benchmark-arguments: ""
    # benchmark-arguments: "--csv bench.csv"
    no-run-benchmarks: false
  force-dirty: false
  reconfigure: false
  cabal-verbose: false
  split-objs: false
```

The meanings of these settings correspond directly with the CLI flags of the
same name. See the [build command docs](build_command.md) and the
[users guide](GUIDE.md#the-build-command) for more info.

### dump-logs

(Since 1.3.0)

Control which log output from local non-dependency packages to print to the
console. By default, Stack will only do this when building a single target
package or if the log contains warnings, to avoid generating unnecessarily
verbose output.

```yaml
dump-logs: none      # don't dump logs even if they contain warnings
dump-logs: warning   # default: dump logs that contain warnings
dump-logs: all       # dump all logs for local non-dependency packages
```

### templates

Templates used with `stack new` have a number of parameters that affect the
generated code. These can be set for all new projects you create. The result of
them can be observed in the generated LICENSE and cabal files.

The 5 parameters are: `author-email`, `author-name`, `category`, `copyright` and `github-username`.

* _author-email_ - sets the `maintainer` property in cabal
* _author-name_ - sets the `author` property in cabal and the name used in
  LICENSE
* _category_ - sets the `category` property in cabal. This is used in Hackage.
  For examples of categories see [Packages by
  category](https://hackage.haskell.org/packages/). It makes sense for
  `category` to be set on a per project basis because it is uncommon for all
  projects a user creates to belong to the same category. The category can be
  set per project by passing `-p "category:value"` to the `stack new` command.
* _copyright_ - sets the `copyright` property in cabal. It is typically the
  name of the holder of the copyright on the package and the year(s) from which
  copyright is claimed. For example: `Copyright (c) 2006-2007 Joe Bloggs`
* _github-username_ - used to generate `homepage` and `source-repository` in
  cabal. For instance `github-username: myusername` and `stack new my-project new-template`
  would result:

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
    copyright: 'Copyright (c) 2017 Your Name'
    github-username: yourusername
```

Additionally, `stack new` can automatically initialize source control repositories
in the directories it creates.  Source control tools can be specified with the
`scm-init` option.  At the moment, only `git` is supported.

```yaml
templates:
  scm-init: git
```

### save-hackage-creds

Controls whether, when using `stack upload`, the user's Hackage
username and password are stored in a local file. Default: true.

```yaml
save-hackage-creds: true
```

Since 1.5.0

### ignore-revision-mismatch

Cabal files in packages can be specified via exact revisions to deal
with Hackage revision metadata. The default behavior of Stack (since
1.6.0) is to fail if an exact match is not found. In some cases
(specifically, when using a legacy `00-index.tar.gz` file), users may
wish to allow a mismatch. In such cases, you can change
`ignore-revision-mismatch` from `false` to `true`.

```yaml
ignore-revision-mismatch: false
```

For more information, see
[the Github issue #3520 discussion](https://github.com/commercialhaskell/stack/issues/3520).

Since 1.6.0

### urls

Customize the URLs where `stack` looks for snapshot build plans.

The default configuration is

```yaml
urls:
  latest-snapshot: https://www.stackage.org/download/snapshots.json
  lts-build-plans: https://raw.githubusercontent.com/fpco/lts-haskell/master/
  nightly-build-plans: https://raw.githubusercontent.com/fpco/stackage-nightly/master/
```

**Note:** The `latest-snapshot-url` field has been deprecated in favor of `latest-snapshot`
and will be removed in a future version of `stack`.

### jobs

Specifies how many build tasks should be run in parallel. This can be overloaded
on the commandline via `-jN`, for example `-j2`.  The default is to use the
number of processors reported by your CPU.  One usage for this might be to avoid
running out of memory by setting it to 1, like this:

```yaml
jobs: 1
```

### work-dir

Specifies relative path of work directory (default is `.stack-work`. This can
also be specified by env var or cli flag, in particular, the earlier items in
this list take precedence:

1. `--work-dir DIR` passed on the commandline
2. `work-dir` in stack.yaml
3. `STACK_WORK` environment variable

Since 0.1.10.0

### skip-msys

Skips checking for and installing msys2 when stack is setting up the
environment.  This is only useful on Windows machines, and usually doesn't make
sense in project configurations, just in `config.yaml`.  Defaults to `false`, so
if this is used, it only really makes sense to use it like this:

```yaml
skip-msys: true
```

Since 0.1.2.0

### concurrent-tests

This option specifies whether test-suites should be executed concurrently with
each-other. The default for this is true, since this is usually fine and it
often means that tests can complete earlier. However, if some test-suites
require exclusive access to some resource, or require a great deal of CPU or
memory resources, then it makes sense to set this to `false` (the default is
`true`).

```yaml
concurrent-tests: false
```

Since 0.1.2.0

### extra-path

This option specifies additional directories to prepend to the PATH environment
variable.  These will be used when resolving the location of executables, and
will also be visible in the `PATH` variable of processes run by stack.

For example, to prepend `/path-to-some-dep/bin` to your PATh:

```yaml
extra-path:
- /path-to-some-dep/bin
```

One thing to note is that other paths added by stack - things like the project's
bin dir and the compiler's bin dir - will take precedence over those specified
here (the automatic paths get prepended).

Since 0.1.4.0

### local-programs-path

This overrides the location of the programs directory, where tools like ghc and
msys get installed.

On most systems, this defaults to a folder called `programs`
within the stack root directory. On windows, if the `LOCALAPPDATA` environment
variable exists, then it defaults to `$LOCALAPPDATA/Programs/stack/`, which
follows windows conventions.

Since 1.3.0

### default-template

This option specifies which template to use with `stack new`, when none is
specified. The default is called `new-template`. The other templates are listed
in [the stack-templates repo](https://github.com/commercialhaskell/stack-templates/).
