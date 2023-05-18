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
  example `stack setup 9.4.4` will attempt to install GHC 9.4.4; and

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

    For example, Stack 2.11.1 considers:

    *   the version of `libc6`, the
        [GNU C Library](https://www.gnu.org/software/libc/) (glibc), that is
        present. The GNU C Library is designed to be backwards compatible.

    *   if `libgmp.so.3` or `libgmp.so.10` is present. These files are provided
        by different versions of the
        [GNU Multiple Precision Arithmetic Library](https://gmplib.org/).

    *   if `libncursesw.so.6` is present. This file is provided by a shared
        library for terminal handling with wide character support.

    *   if `libtinfo.so.5` or `libtinfo.so.6` is present. These files are
        provided by different versions of a shared low-level terminfo library
        for terminal handling.

    Stack 2.11.1 uses `ghc-build`:

    * `tinfo6` to indicate `libgmp.so.10` and `libtinfo.so.6` are present and
      `libc6` is compatible with `libc6` 2.32.
    * `tinfo6-libc6-pre232` to indicate `libgmp.so.10` and `libtinfo.so.6` are
       present and `libc6` is not compatible with `libc6` 2.32.
    * `ncurses6` to indicate `libgmp.so.10` and `libncursesw.so.6` are present
    * `gmp4` to indicate `libgmp.so.3` is present

    By default, Stack associates:

    * the `tinfo6` build with the 'Fedora 33' binary distribution of GHC 9.4.1
      to 9.4.4. Those binary distributions require versions of `libc6` that are
      compatible with `libc6` 2.32; and

    * the `tinfo6-libc6-pre232` build with the 'Debian 10' binary distribution
      of GHC 9.4.1 to 9.4.4. Those binary distributions require versions of
      `libc6` that are compatible with `libc6` 2.28.
