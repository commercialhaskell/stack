<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Upgrading msys

When installing GHC on Windows, Stack will also install
[msys2](http://www.msys2.org/) to provide a Unix shell and environment,
necessary for such things as running configure scripts. This section explains
the steps required to upgrade the msys2 version used by Stack.

1.  Download latest installers from msys2's website. These installers are
    executables, versioned by date (YYYYMMDD), and are separate for `x86_64`
    and `i686`. You'll usually be upgrading both at the same time, which we'll
    assume here.

2.  Run the installer and install to the default location (`c:\msys64` and
    `c:\msys32`, respectively).

3.  Create tarballs for each directory:

    ```
    $ cd /c/
    $ tar cJf msys2-YYYYMMDD-x86_64.tar.xz msys64
    $ tar cJf msys2-YYYYMMDD-i686.tar.xz msys32
    ```

4.  Create a new release named `msys2-YYYYMMDD` on the
    [fpco/stackage-content](https://github.com/fpco/stackage-content)
    repo, and upload these two files.

5.  Create a PR for the [stack-setup-2.yaml file](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml)
    to switch over to using the newly uploaded files. You should test this file locally first.
