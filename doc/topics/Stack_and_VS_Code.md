<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack and Visual Studio Code

[Visual Studio Code](https://code.visualstudio.com/) (VS Code) is a popular
source code editor, and
['Haskell'](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
is an extension for VS Code that is popular with Haskell coders.

The 'Haskell' extension can be used with Stack but there are some things to be
aware of, set out below.

## GHCup

The separate [GHCup](https://www.haskell.org/ghcup/) project provides a tool
that can be used to install various tools useful for developing Haskell
projects. Those tools include:

* GHC,
* HLS (see further below),
* MSYS2 (on Windows; see the developing on Windows
  [documentation](developing_on_windows.md)),
* Stack, and
* Cabal (the tool).

Stack itself can be used to install GHC and MSYS2. Stack can also be used to
upgrade, or downgrade, Stack.

GHCup can configure Stack so that if Stack needs a version of GHC, GHCup takes
over obtaining and installing that version. By default, the script to install
GHCup (which can be run more than once) configures Stack in that way. For
further information about how GHCup configures Stack, see the GHC installation
customisation
[documentation](../configure/customisation_scripts.md#ghc-installation-customisation).

On Windows, GHCup has the capability of using the Stack-supplied MSYS2 rather
than installing a duplicate copy.

## HLS

The VS Code extension makes use of
[HLS](https://github.com/haskell/haskell-language-server) (the Haskell Language
Server). To work, HLS has to be built with the same version of GHC that it will
support. That is, a version of HLS is required for each version of GHC in use.
It is possible that the most recent versions of GHC are not supported by HLS.

VS Code with the 'Haskell' extension can be configured in a number of ways:

=== "GHCup manages HLS"

    The VS Code extension's settings (under 'Haskell: Manage HLS') allow a
    user to specify that the extension should use GHCup, to download and install
    the versions of HLS that it needs.

    If GHCup manages versions of HLS, versions of GHC can be managed in a number
    of ways:

    === "Stack manages GHC using GHCup"

        As identified above, GHCup can configure Stack to use GHCup to manage
        versions of GHC.

    === "Stack manages GHC directly"

        It is possible to install GHCup so that it is 'empty' except for the
        current version of HLS, allow the VS Code extension to use GHCup to
        manage HLS requirements only, and to disable messages from the extension
        on start-up that installation of GHC, Cabal (the tool)
        and/or Stack are also necessary (they are not, if only Stack is being
        used).

        To install a version of GHCup that is 'empty' is a little more
        complicated than a default installation of GHCup.

        === "Unix-like"

            The following environment variable must be set before GHCup's
            installation `sh` script is run: `BOOTSTRAP_HASKELL_MINIMAL`.

        === "Windows"

            The second argument to the PowerShell script must be set to
            `$false`, namely:

                Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true,$false

        There is the possibility of false messages from the extension on
        start-up that need to be ignored. Those messages can be disabled by
        [setting the following](https://github.com/haskell/vscode-haskell#setting-a-specific-toolchain)
        for the VS Code extension:

        ~~~yaml
        "haskell.toolchain": {
          "ghc": null,
          "cabal": null,
          "stack": null
        }
        ~~~

        There can be no differences between the version of GHC that the
        GHCup-supplied HLS was built with and the version that Stack has
        installed.

        For the most part, the versions of HLS provided by GHCup are built with
        the same versions of GHC that Stack downloads from its default
        `setup-info` dictionary (see the
        [`setup-info`](../configure/yaml/non-project.md#setup-info) non-project
        specific configuration option documentation). Stack's default is to
        mirror the 'official' binary distributions published by GHC.

        However, in some cases, it is possible that a GHCup-supplied and
        GHCup-selected HLS has been built with a different binary distribution
        of GHC than the one which Stack has installed.

        ??? question "When have the GHCup- and Stack-supplied GHCs differed?"

            An example occurred with the release of GHC 9.0.2. For some Linux
            users (Debian 9 and Fedora 27), the version of GHC 9.0.2 linked on
            GHC’s download
            [web page](https://www.haskell.org/ghc/download_ghc_9_0_2.html) was
            broken. The GHC developers made alternative ‘9.0.2a’ versions
            available. For a while, Stack referred to the versions published by
            GHC on its download web page while the GHCup-supplied versions of
            HLS were built using alternative versions. This incompatibility led
            to problems.

            It was resolved by Stack's default also being changed to refer to
            the '9.0.2a' versions. Where Stack has already installed GHC 9.0.2,
            it is necessary to delete GHC 9.0.2 from the `stack path --programs`
            directory. This will cause Stack to reinstall the alternative
            version, when it first needs GHC 9.0.2. Stack should distinguish
            what it builds with the alternative from what it has built, and
            cached, with the original GHC 9.0.2.

    === "Stack uses a GHCup-supplied GHC"

        GHCup is used to manage versions of GHC and Stack is configured to use
        the version of GHC on the PATH.

        That is, GHCup is used to install a version of GHC on the PATH. Stack is
        configured to make use of that version, by making use of Stack's
        `install-ghc` option (which needs to be disabled) and Stack's
        `system-ghc` option (which needs to be enabled).

        For further information about these options, see the
        [`install-ghc`](../configure/yaml/non-project.md#install-ghc)
        documentation and the
        [`system-ghc`](../configure/yaml/non-project.md#system-ghc)
        documentation.

        Each time that a snapshot is used that references a different version of
        GHC, then GHCup must be used to install it (if GHCup has not already
        installed that version). For example, to use `snapshot: lts-22.43`
        (GHC 9.6.6), the command `ghcup install ghc 9.6.6` must have been used
        to install GHC 9.6.6. That may be a minor inconvenience for some people,
        as one the primary benefits of Stack over other tools for building
        Haskell code has been that Stack automatically ensures that the
        necessary version of GHC is available.

=== "User manages HLS"

    By default, the VS Code extension uses tools that are in the PATH.

    If the VS Code extension is set not to use GHCup, its user needs to ensure
    that each version of HLS that the extension needs is on the PATH.

### Cradle

HLS may need a 'cradle' - an
[`hie.yaml` file](https://hackage.haskell.org/package/hie-bios#stack) - in the
project's root directory in order to work well.

The [`gen-hie` tool](https://hackage.haskell.org/package/implicit-hie) can help
generate such a cradle.

### Tips

It has been suggested that a project must have been successfully built before
the VS code extension (and HLS) is first activated on the project, for HLS to
work reliably.
