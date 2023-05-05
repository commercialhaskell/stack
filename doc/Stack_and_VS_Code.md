<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack and Visual Studio Code

[Visual Studio Code](https://code.visualstudio.com/) (VS Code) is a popular
source code editor, and
['Haskell'](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
is an extension for VS Code that is popular with Haskell coders.

The 'Haskell' extension can be used with Stack but there are some things to be
aware of, set out below.

## Haskell Language Server

The VS Code extension makes use of the Haskell Language Server (HLS). To work,
HLS has to be built with the same version of GHC that it will support. That is,
a version of HLS is required for each version of GHC in use. It is possible that
the most recent versions of GHC are not supported by HLS.

By default, the VS Code extension uses tools that are in the PATH. However, the
extension's settings (under 'Haskell: Manage HLS') allow a user to specify
that the extension should use a separate application,
[GHCup](https://www.haskell.org/ghcup/), to download and install the versions of
HLS that it needs. GHCup can download and install things other than HLS,
including GHC, MSYS2 (on Windows), Cabal (a build tool), and Stack itself. GHCup
can also update itself. On Windows, GHCup has the capability of using the
Stack-supplied MSYS2 rather than installing a duplicate copy. Cabal (the build
tool), like Stack, depends on the Cabal (the library). Cabal (the tool), unlike
Stack, does not have the capability to automatically install necessary versions
of GHC, and (as well as supporting the extension) GHCup fills a important gap
for users of the Cabal tool.

If the VS Code extension is set not to use GHCup, its user needs to ensure that
each version of HLS that the extension needs is on the PATH.

For the most part, the versions of HLS provided by GHCup are built with the same
versions of GHC that Stack downloads from its default `setup-info` dictionary
(see [YAML configuration: setup-info](yaml_configuration.md)). Stack's default
is to mirror the 'official' binary distributions published by GHC. However, in
some cases, it is possible that a GHCup-supplied and GHCup-selected HLS has been
built with a different binary distribution of GHC than the one which Stack has
installed.

One example of that occurred with the release of GHC 9.0.2. For some Linux users
(Debian 9 and Fedora 27), the version of GHC 9.0.2 linked on GHC’s download
[web page](https://www.haskell.org/ghc/download_ghc_9_0_2.html) was broken. The
GHC developers made alternative ‘9.0.2a’ versions available. For a while, Stack
referred to the versions published by GHC on its download web page while the
GHCup-supplied versions of HLS were built using alternative versions. This
incompatibility led to problems. It was resolved by Stack's default also being
changed to refer to the '9.0.2a' versions. (Where Stack has already installed
GHC 9.0.2, it is necessary to delete GHC 9.0.2 from the `stack path --programs`
directory. This will cause Stack to reinstall the alternative version, when it
first needs GHC 9.0.2. Stack should distinguish what it builds with the
alternative from what it has built, and cached, with the original GHC 9.0.2.)

### GHCup and Stack >= 2.9.1

From Stack 2.9.1, GHCup can configure Stack so that if Stack needs a version of
GHC, GHCup takes over obtaining and installing that version. By default, the
script to install GHCup (which can be run more than once) configures Stack in
that way. For further information about how GHCup configures Stack, see the GHC
installation customisation
[documentation](yaml_configuration.md#ghc-installation-customisation).

### Workaround #1

If GHCup does not configure Stack in the way described above, one workaround is
to allow GHCup to install versions of GHC on the PATH and to cause Stack to use
those versions of GHC, by making use of Stack's `install-ghc` option (which
needs to be disabled) and Stack's `system-ghc` option (which needs to be
enabled). For further information about these options, see the `install-ghc`
[documentation](yaml_configuration.md#install-ghc) and the `system-ghc`
[documentation](yaml_configuration.md#system-ghc).

For this workaround to work, each time that a resolver is used that references a
different version of GHC, then GHCup must be used to install it (if GHCup has
not already installed that version). For example, to use `resolver: lts-20.19`
(GHC 9.2.7), the command `ghcup install ghc 9.2.7` must have been used to
install GHC 9.2.7. That may be a minor inconvenience for some people, as one the
primary benefits of Stack over other Haskell build tools has been that Stack
automatically ensures that the necessary version of GHC is available.

### Workaround #2

If GHCup does not configure Stack, another partial workaround is to install
GHCup so that it is 'empty' except for the current version of HLS, allow the
VS Code extension to use GHCup to manage HLS requirements only, and to ignore
any messages (if any) from the extension on start-up that installation of GHC,
Cabal (the tool) and/or Stack are also necessary (they are not, if only Stack is
being used).

For this workaround to work, however, there can be no differences between the
version of GHC that the GHCup-supplied HLS was built with and the version that
Stack has installed. A slight inconvenience here is also the possibility of
false messages from the start-up that need to be ignored. In principle, those
messages can be disabled by
[setting the following](https://github.com/haskell/vscode-haskell#setting-a-specific-toolchain)
for the VS Code extension:

~~~yaml
"haskell.toolchain": {
  "ghc": null,
  "cabal": null,
  "stack": null
}
~~~

To install a version of GHCup that is 'empty' is a little more complicated than
a default installation of GHCup.

On Unix-like operating systems, the following environment variable must be set
before GHCup's installation `sh` script is run: `BOOTSTRAP_HASKELL_MINIMAL`.

On Windows, the second argument to the PowerShell script must be set to
`$false`, namely:

    Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true,$false

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
