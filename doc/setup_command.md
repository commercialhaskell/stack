<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack setup` command

~~~text
stack setup [GHC_VERSION] [--[no-]reinstall] [--ghc-bindist URL]
            [--ghcjs-boot-options GHCJS_BOOT] [--[no-]ghcjs-boot-clean]
~~~

`stack setup` attempts to install a version of GHC.

By default:

* the version of GHC is the one required by the project. Specify the version of
  GHC as an argument to attempt to install a different version of GHC. For
  example `stack setup 9.4.3` will attempt to install GHC 9.4.3; and

* an attempt to install is made only if the version of GHC is not already
  available to Stack. Pass the flag `--reinstall` (disabled by default) to
  attempt to install the version of GHC regardless of whether it is already
  available to Stack.

Pass the option `--ghc-bindist <url>` to specify the URL of the GHC to be
downloaded and installed. This option requires the use of the `--ghc-variant`
option specifying a custom GHC variant. For further information about the
`--ghc-variant` option, see the see the
[YAML configuration](yaml_configuration.md#ghc-variant) documentation.

If Stack is configured not to install GHC (`install-ghc: false` or passing the
`--no-install-ghc` flag) then `stack setup` will warn that the flag and the
command are inconsistent and take no action.

=== "Linux"

    A particular binary distribution of GHC will depend on certain libraries,
    which need to be available.

    There are many different Linux distributions and different versions of a
    particular Linux distribution. One Linux distribution/version may make
    available different libraries to another Linux distribution/version.

    In attempting to identify the particular binary distribution of GHC that is
    required on Linux, Stack will refer to the presence or absence of certain
    libraries or the versions of those libraries.

    For example, Stack 2.9.3 considers:

    *   if `libgmp.so.3` or `libgmp.so.10` is present. These files are provided
        by different versions of the
        [GNU Multiple Precision Arithmetic Library](https://gmplib.org/).

    *   if `libncursesw.so.6` is present. This file is provided by a shared
        library for terminal handling with wide character support.

    *   if `libtinfo.so.5` or `libtinfo.so.6` is present. These files are
        provided by different versions of a shared low-level terminfo library
        for terminal handling.

    Stack 2.9.3 uses `ghc-build`:

    * `tinfo6` to indicate `libgmp.so.10` and `libtinfo.so.6` are present
    * `ncurses6` to indicate `libgmp.so.10` and `libncursesw.so.6` are present
    * `gmp4` to indicate `libgmp.so.3` is present

    GHC also depends on `libc6`, the GNU C Library (glibc). The GNU C Library is
    designed to be backwards compatible.

    By default, versions of Stack up to 2.9.3 associate the `tinfo6` build with
    the 'Fedora 33' binary distribution of GHC 9.4.1 to 9.4.3. Those binary
    distributions require versions of `libc6` that are compatible with
    `libc6` 2.32.

    For Linux distributions/versions that provide versions of `libc6` that are
    not compatible with `libc6` 2.32 but are compatible with `libc6` 2.28, the
    'Debian 10' binary distributions of GHC 9.4.1 to 9.4.3 may be a viable
    alternative. They can be specified using a `setup-info` key in a Stack
    YAML configuration file (global or project-level), as follows:

    ~~~yaml
    setup-info:
      ghc:
        linux64-tinfo6:
          9.4.1:
            url: "https://downloads.haskell.org/~ghc/9.4.1/ghc-9.4.1-x86_64-deb10-linux.tar.xz"
            content-length: 186574068
            sha1: 7e885cae97fbf893d8d3e41e19131a6c73264941
            sha256: dcbff828b14a59d01d3fda68bb01b9cbc3a321a0c013905f436df5627128aa58
          9.4.2:
            url: "https://downloads.haskell.org/~ghc/9.4.2/ghc-9.4.2-x86_64-deb10-linux.tar.xz"
            content-length: 184995132
            sha1: 004f5aec6ab60ca95ddd78fa2b99d6d7657e6a06
            sha256: 5bf34ef70a2b824d45e525f09690c76707b7f01698962e425e8fd78b94ea9174
          9.4.3:
            url: "https://downloads.haskell.org/~ghc/9.4.3/ghc-9.4.3-x86_64-deb10-linux.tar.xz"
            content-length: 184837364
            sha1: 28a94f2da6077d725a7d72f48bc909cf9f2444e3
            sha256: 940ac2b1770dc63b5f3f38f829bfe69f4a572d6b26cd93094cdd99d5300b5067
    ~~~

    Examples of such Linux distributions/versions are Debian 10 itself,
    Debian 11 (`libc6` 2.31), and Ubuntu 20.04 (`libc6` 2.31).
