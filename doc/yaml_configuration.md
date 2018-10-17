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

*Note 2:* A common source of confusion is the distinction between configuration
in a `stack.yaml` file versus a cabal file. If you're trying to understand this
breakdown, see [stack vs cabal config](stack_yaml_vs_cabal_package_file.md).

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

> Note: Starting with **Stack 2.0**, `snapshot` is accepted as a synonym for `resolver`. Only one of these fields is permitted, not both.

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
* Custom snapshot, via a URL or relative file path. (See [pantry docs](pantry.md) for more information.)

Each of these resolvers will also determine what constraints are placed on the
compiler version. See the [compiler-check](#compiler-check) option for some
additional control over compiler version.

Since Stack 1.11, the resolver field corresponds to a Pantry snapshot
location. See [the docs on pantry](pantry.md) for more information.

### packages

_NOTE_ Beginning with Stack 1.11, Stack has moved over to Pantry for
managing extra-deps, and has removed some legacy syntax for specifying
dependencies in `packages`. See some conversion notes below.

A list of packages that are part of your local project. These are
specified via paths to local directories. The paths are considered
relative to the directory containing the `stack.yaml` file. For
example, if your `stack.yaml` is located at `/foo/bar/stack.yaml`, and
you have:

```yaml
packages:
- hello
- there/world
```

Your configuration means "I have packages in `/foo/bar/hello` and
`/foo/bar/there/world`.

If these packages should be treated as dependencies instead, specify
them in `extra-deps`, described below.

The `packages` field is _optional_. If omitted, it is treated as:

```yaml
packages:
- .
```

Each package directory specified must have a valid cabal file or hpack
`package.yaml` file present. Note that the subdirectories of the
directory are not searched for cabal files. Subdirectories will have
to be specified as independent items in the list of packages.

Meaning that your project has exactly one package, and it is located
in the current directory.

Project packages are different from snapshot dependencies (via
`resolver`) and extra dependencies (via `extra-deps`) in multiple
ways, e.g.:

* Project packages will be built by default with a `stack build`
  without specific targets. Dependencies will only be built if
  they are depended upon.
* Test suites and benchmarks may be run for project packages. They are
  never run for extra dependencies.

__Legacy syntax__ Prior to Stack 1.11, it was possible to specify
dependencies in your `packages` configuration value as well. This
support has been removed to simplify the file format. Instead, these
values should be moved to `extra-deps`. As a concrete example, you
would convert:

```yaml
packages:
- .
- location:
    git: https://github.com/bitemyapp/esqueleto.git
    commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  extra-dep: true
- location:
    git: https://github.com/yesodweb/wai.git
    commit: 6bf765e000c6fd14e09ebdea6c4c5b1510ff5376
    subdirs:
      - wai-extra
  extra-dep: true

extra-deps:
  - streaming-commons-0.2.0.0
  - time-1.9.1
  - yesod-colonnade-1.3.0.1
  - yesod-elements-1.1
```

into

```yaml
packages:
- .

extra-deps:
  - streaming-commons-0.2.0.0
  - time-1.9.1
  - yesod-colonnade-1.3.0.1
  - yesod-elements-1.1
  - git: https://github.com/bitemyapp/esqueleto.git
    commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  - git: https://github.com/yesodweb/wai.git
    commit: 6bf765e000c6fd14e09ebdea6c4c5b1510ff5376
    subdirs:
      - wai-extra
```

And, in fact, the `packages` value could be left off entirely since
it's using the default value.

### extra-deps

This field allows you to specify extra dependencies on top of what is
defined in your snapshot (specified in the `resolver` field mentioned
above). These dependencies may either come from a local file path or a
Pantry package location.

For the local file path case, the same relative path rules as apply to
`packages` apply.

Pantry package locations allow you to include dependencies from three
different kinds of sources:

* Hackage
* Archives (tarballs or zip files, either local or over HTTP(S))
* Git or Mercurial repositories

Here's an example using all of the above:

```yaml
extra-deps:
- vendor/hashable
- streaming-commons-0.2.0.0
- time-1.9.1
- yesod-colonnade-1.3.0.1
- yesod-elements-1.1
- git: https://github.com/bitemyapp/esqueleto.git
  commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
- url: https://github.com/yesodweb/wai/archive/6bf765e000c6fd14e09ebdea6c4c5b1510ff5376.tar.gz
  subdirs:
    - wai-extra
- github: snoyberg/conduit
  commit: 2e3e41de93821bcfe8ec6210aeca21be3f2087bf
  subdirs:
    - network-conduit-tls
```

If no `extra-deps` value is provided, it defaults to an empty list,
e.g.:

```yaml
extra-deps: []
```

For more information on the format for specifying dependencies, please
see [the Pantry docs](pantry.md).

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

Since Stack 1.11, this field may only be used to specify a single
package index, which must use the Hackage Security format. For the
motivation for this change, please see [issue #4137](https://github.com/commercialhaskell/stack/issues/4137).
Therefore, this field is most useful for providing an alternate
Hackage mirror either for:

* Bypassing a firewall
* Faster download speeds

The following is the default setting for this field:

```yaml
package-indices:
- download-prefix: https://hackage.haskell.org/
  hackage-security:
    keyids:
    - 0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d
    - 1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42
    - 280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833
    - 2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201
    - 2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3
    - 51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921
    - 772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d
    - aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9
    - fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0
    key-threshold: 3 # number of keys required
```

If you provide a replacement index which does not mirror Hackage, it
is likely that you'll end up with significant breakage, such as most
snapshots failing to work.

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
for reasoning). This can lead to unpredictable behavior by affecting
your snapshot packages.

The behavior of the `$locals`, `$targets`, and `$everything` special
keys mirrors the behavior for the
[`apply-ghc-options` setting](#apply-ghc-options), which affects
command line parameters.

NOTE: Prior to version 1.6.0, the `$locals`, `$targets`, and
`$everything` keys were not supported. Instead, you could use `"*"` for
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
  keep-tmp-files: false

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

  # Since 1.8
  interleaved-output: false

  # Since 1.10
  ddump-dir: ""
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
them can be observed in the generated LICENSE and cabal files. The value for all
of these parameters must be strings.

The parameters are: `author-email`, `author-name`, `category`, `copyright`, `year` and `github-username`.

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
* _year_ - if `copyright` is not specified, `year` and `author-name` are used
  to generate the copyright property in cabal. If `year` is not specified, it
  defaults to the current year.
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
    copyright: 'Copyright (c) 2018 Your Name'
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

### hackage-base-url

Sets the address of the Hackage server to upload the package to. Default is
`https://hackage.haskell.org/`.

```yaml
hackage-base-url: https://hackage.example.com/
```

Since 1.9.1

### ignore-revision-mismatch

This flag was introduced in Stack 1.6, and removed in Stack 1.11 with
the move to Pantry. You will receive a warning if this configuration
value is set.

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

For example, to prepend `/path-to-some-dep/bin` to your PATH:

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

### color

This option specifies when to use color in output. The option is used as
`color: <WHEN>`, where `<WHEN>` is 'always', 'never', or 'auto'. On Windows
versions before Windows 10, for terminals that do not support color codes, the
default is 'never'; color may work on terminals that support color codes.

The color use can also be set at the command line using the equivalent
`--color=<WHEN>` global option. Color use set at the command line takes
precedence over that set in a yaml configuration file.

(The British English spelling (colour) is also accepted. In yaml configuration
files, the American spelling is the alternative that has priority.)

### stack-colors

Stack uses styles to format some of its output. The default styles do not work
well with every terminal theme. This option specifies stack's output styles,
allowing new styles to replace the defaults. The option is used as
`stack-colors: <STYLES>`, where `<STYLES>` is a colon-delimited sequence of
key=value, 'key' is a style name and 'value' is a semicolon-delimited list of
'ANSI' SGR (Select Graphic Rendition) control codes (in decimal). Use the
command `stack ls stack-colors --basic` to see the current sequence.

The 'ANSI' standards refer to (1) standard ECMA-48 'Control Functions for Coded
Character Sets' (5th edition, 1991); (2) extensions in ITU-T Recommendation
(previously CCITT Recommendation) T.416 (03/93) 'Information Technology – Open
Document Architecture (ODA) and Interchange Format: Character Content
Architectures' (also published as ISO/IEC International Standard 8613-6); and
(3) further extensions used by 'XTerm', a terminal emulator for the X Window
System. The 'ANSI' SGR codes are described in a
[Wikipedia article](http://en.wikipedia.org/wiki/ANSI_escape_code)
and those codes supported on current versions of Windows in
[Microsoft's documentation](https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences).

For example, users of the popular
[Solarized Dark](https://ethanschoonover.com/solarized/)
terminal theme might wish to set the styles as follows:

```yaml
stack-colors: error=31:good=32:shell=35:dir=34:recommendation=32:target=95:module=35:package-component=95
```
The styles can also be set at the command line using the equivalent `--stack-colors=<STYLES>`
global option. Styles set at the command line take precedence over those set in
a yaml configuration file.

(The British English spelling (colour) is also accepted. In yaml configuration
files, the American spelling is the alternative that has priority.)
