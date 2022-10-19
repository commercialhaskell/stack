<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Install or upgrade

## Install Stack

Stack can be installed on most Linux distributions, macOS and Windows.

Stack is open to supporting more operating systems. To request support for an
operating system, please submit an
[issue](https://github.com/commercialhaskell/stack/issues/new) at Stack's
GitHub repository.

!!! info

    In addition to the methods described below, Stack can also be installed
    using the separate [GHCup](https://www.haskell.org/ghcup/) installer for
    Haskell-related tools. GHCup provides Stack for some combinations of machine
    architecture and operating system not provided elsewhere. Unlike Stack,
    other build tools do not automatically install GHC. GHCup can be used to
    install GHC for those other tools. By default, the script to install GHCup
    (which can be run more than once) also configures Stack so that if Stack
    needs a version of GHC, GHCup takes over obtaining and installing that
    version.

!!! info "Releases on GitHub"

    Stack executables are also available on the
    [releases](https://github.com/commercialhaskell/stack/releases) page of
    Stack's GitHub repository.

!!! info "`https://get.haskellstack.org/stable` URLs"

    URLs with the format
    `https://get.haskellstack.org/stable/<PLATFORM>.<EXTENSION>` point to the
    latest stable release. See the manual download links for examples.

=== "Linux"

    For most Linux distributions, the easiest way to install Stack is to
    command:

    ~~~text
    curl -sSL https://get.haskellstack.org/ | sh
    ~~~

    or:

    ~~~text
    wget -qO- https://get.haskellstack.org/ | sh
    ~~~

    !!! note

        The script at [get.haskellstack.org](https://get.haskellstack.org/) will
        ask for root access using `sudo`. It needs such access in order to use
        your platform's package manager to install dependencies and to install
        to `/usr/local/bin`. If you prefer more control, follow the manual
        installation instructions for your platform below.

    ### Manual download

    Manual download for Linux distributions depends on your machine
    architecture, x86_64 or AArch64/ARM64.

    === "x86_64"

        * Click
          [:material-cloud-download-outline:](https://get.haskellstack.org/stable/linux-x86_64.tar.gz)
          to download an archive file with the latest release.

        * Extract the archive and place the `stack` executable somewhere on your
          PATH (see the [Path](#path) section below).

        * Ensure you have the required system dependencies installed. These
          include GCC, GNU Make, xz, perl, libgmp, libffi, and zlib. We also
          recommend Git and GPG.

        The installation of system dependencies will depend on the package
        manager for your Linux distribution. Notes are provided for Arch Linux,
        CentOS, Debian, Fedora, Gentoo and Ubuntu.

        === "Arch Linux"

            ~~~text
            sudo pacman -S make gcc ncurses git gnupg xz zlib gmp libffi zlib
            ~~~

        === "CentOS"

            ~~~text
            sudo yum install perl make automake gcc gmp-devel libffi zlib zlib-devel xz tar git gnupg
            ~~~

        === "Debian"

            ~~~text
            sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase
            ~~~

        === "Fedora"

            ~~~text
            sudo dnf install perl make automake gcc gmp-devel libffi zlib zlib-devel xz tar git gnupg
            ~~~

        === "Gentoo"

            Ensure you have the `ncurses` package with `USE=tinfo`. Without it,
            Stack will not be able to install GHC.

        === "Ubuntu"

            ~~~text
            sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase
            ~~~

    ### Linux packages

    Some Linux distributions have official or unofficial packages for Stack,
    including Arch Linux, Debian, Fedora, NixOS, openSUSE/SUSE Linux Enterprise,
    and Ubuntu.

    !!! info "Linux packages that lag behind Stack's current version"

        The Stack version available as a Linux package may lag behind Stack's
        current version. If so, using `stack upgrade --binary-only` is
        recommended after installing it. For Stack versions before 1.3.0 which
        do not support `--binary-only`, just `stack upgrade` may work too.

    === "Arch Linux"

        The Arch community package repository provides an official
        [package](https://www.archlinux.org/packages/community/x86_64/stack/).
        You can install it with the command:

        ~~~text
        sudo pacman -S stack
        ~~~

        This version may slightly lag behind, but it should be updated within
        the day. The package is also always rebuilt and updated when one of its
        dependencies gets an update.

        The Arch User Repository (AUR) also provides a
        [package](https://aur.archlinux.org/packages/haskell-stack-git).
        However, its Stack version lags behind, so running
        `stack upgrade --binary-only` is recommended after installing it. For
        older Stack versions which do not support `--binary-only`, just
        `stack upgrade` may work too.

        To use `stack setup` with versions of GHC before 7.10.3 or on a
        32-bit system, you may need the AUR
        [ncurses5-compat-libs](https://aur.archlinux.org/packages/ncurses5-compat-libs/)
        package installed.

    === "Debian"

        There are Debian
        [packages](https://packages.debian.org/search?keywords=haskell-stack&searchon=names&suite=all&section=all)
        for Stretch and up. However, the distribution's Stack version lags
        behind.

    === "Fedora"

        Fedora includes Stack, but its Stack version may lag behind.

    === "NixOS"

        Users who follow the `nixos-unstable` channel or the Nixpkgs `master`
        branch can install the latest Stack release into their profile with the
        command:

        ~~~text
        nix-env -f "<nixpkgs>" -iA stack
        ~~~

        Alternatively, the package can be built from source as follows.

        1.  Clone the git repo, with the command:

            ~~~text
            git clone https://github.com/commercialhaskell/stack.git
            ~~~

        2.  Create a `shell.nix` file with the command:

            ~~~text
            cabal2nix --shell ./. --no-check --no-haddock > shell.nix
            ~~~

            Note that the tests fail on NixOS, so disable them with
            `--no-check`. Also, Haddock currently doesn't work for Stack, so
            `--no-haddock` disables it.

        3.  Install Stack to your user profile with the command:

            ~~~text
            nix-env -i -f shell.nix
            ~~~

        For more information on using Stack together with Nix, please see the
        [NixOS manual section on Stack](http://nixos.org/nixpkgs/manual/#how-to-build-a-haskell-project-using-stack).

    === "SUSE"

        There is also an unofficial package for openSUSE or SUSE Linux
        Enterprise. Its Stack version may lag behind. To install it:

        === openSUSE Tumbleweed

            ~~~text
            sudo zypper in stack
            ~~~

        === openSUSE Leap

            ~~~text
            sudo zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/openSUSE_Leap_42.1/devel:languages:haskell.repo
            sudo zypper in stack
            ~~~

        === SUSE Linux Enterprise 12

            ~~~text
            sudo zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/SLE_12/devel:languages:haskell.repo
            sude zypper in stack
            ~~~

    === "Ubuntu"

        There are Ubuntu
        [packages](http://packages.ubuntu.com/search?keywords=haskell-stack&searchon=names&suite=all&section=all)
        for Ubuntu 18.04 and up. However, the distribution's Stack version lags
        behind.

    It is possible to set up auto-completion of Stack commands. For further
    information, see the [shell auto-completion](shell_autocompletion.md)
    documentation.

=== "macOS"

    The easiest way to install Stack is to command:

    ~~~text
    curl -sSL https://get.haskellstack.org/ | sh
    ~~~

    or:

    ~~~text
    wget -qO- https://get.haskellstack.org/ | sh
    ~~~

    !!! note

        The script at [get.haskellstack.org](https://get.haskellstack.org/) will
        ask for root access using `sudo`. It needs such access in order to use
        your platform's package manager to install dependencies and to install
        to `/usr/local/bin`. If you prefer more control, follow the manual
        installation instructions below.

    !!! info

        We generally test on the current version of macOS and do our best to
        keep it compatible with the three most recent major versions. Stack may
        also work on older versions.

    ### Manual download

    * Click
      [:material-cloud-download-outline:](https://get.haskellstack.org/stable/osx-x86_64.tar.gz)
      to download an archive file with the latest release for x86_64
      architectures.

    * Extract the archive and place `stack` somewhere on your PATH (see the
      [Path](#path) section below).

    * Now you can run Stack from the command line in a terminal.

    ### Using Homebrew

    [Homebrew](https://brew.sh/) is a popular package manager for macOS. If you
    have its `brew` tool installed, you can just command:

    ~~~text
    brew install haskell-stack
    ~~~

    * The Homebrew formula and bottles are **unofficial** and lag slightly
      behind new Stack releases, but tend to be updated within a day or two.

    * Normally, Homebrew will install from a pre-built binary (aka "pour from a
      bottle"), but if it starts trying to build everything from source (which
      will take hours), see
      [their FAQ on the topic](https://github.com/Homebrew/brew/blob/master/docs/FAQ.md#why-do-you-compile-everything).

    ### Notes

    After installation, running `stack setup` might fail with
    `configure: error: cannot run C compiled programs.` in which case you should
    command:

    ~~~text
    xcode-select --install
    ~~~

    Starting with macOs 10.14 (Mojave) running `xcode-select --install`
    [might not be enough](https://forums.developer.apple.com/thread/104296). You
    will need to install additional headers with commands:

    ~~~text
    cd /Library/Developer/CommandLineTools/Packages/
    open macOS_SDK_headers_for_macOS_10.14.pkg
    ~~~

    If you are on OS X 10.11 (El Capitan) and encounter either of these problems,
    see the linked FAQ entries:

    * [GHC 7.8.4 fails with `/usr/bin/ar: permission denied`](faq.md#usr-bin-ar-permission-denied)
    * [DYLD_LIBRARY_PATH is ignored](faq.md#dyld-library-path-ignored)

    If you are on macOS 10.12 (Sierra) and encounter GHC panic while building, see
    this [issue](https://github.com/commercialhaskell/stack/issues/2577)

    On Apple silicon chip (AArch64/ARM64) architectures, the installation of
    Stack or some packages (e.g. `network`) requiring C source compilation might
    fail with `configure: error: C compiler cannot build executables`. In that
    case you should pass `-arch arm64` as part of the `CFLAGS` environment
    variable. This setting will be picked up by the C compiler of your choice.

    ~~~bash
    # Assuming BASH below

    # passing CFLAGS in-line with the command giving rise to the error
    CFLAGS="-arch arm64 ${CFLAGS:-}" some_command_to_install_stack
    CFLAGS="-arch arm64 ${CFLAGS:-}" stack [build|install]

    # -- OR --

    # ~/.bash_profile
    # NOTE: only do this if you do not have to cross-compile, or remember to unset
    # CFLAGS when needed
    export CFLAGS="-arch arm64 ${CFLAGS:-}"
    ~~~

    The setting instructs the C compiler to compile objects for ARM64. These can
    then be linked with libraries built for ARM64. Without the instruction, the C
    compiler, invoked by Cabal running in x86-64, would compile x86-64 objects and
    attempt to link them with existing ARM64 libraries, resulting in the error
    above.

    It is possible to set up auto-completion of Stack commands. For further
    information, see the [shell auto-completion](shell_autocompletion.md)
    documentation.

=== "Windows"

    On 64-bit Windows, you can download and install the
    [Windows installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

    !!! note "Anti-virus software"

        Systems with antivirus software may need to add Stack to the list of
        'trusted' applications.

        You may see a "Windows Defender SmartScreen prevented an unrecognized
        app from starting" warning when you try to run the installer. If so,
        click on **More info**, and then click on the **Run anyway** button that
        appears.

    We recommend installing to the default location with the installer, as that
    will make `stack install` and `stack upgrade` work correctly out of the box.

    ### Manual download

    * Click
      [:material-cloud-download-outline:](https://get.haskellstack.org/stable/windows-x86_64.zip)
      to download an archive file with the latest release.

    * Unpack the archive and place `stack.exe` somewhere on your PATH (see the
      [Path](#path) section below).

    * Now you can run Stack from the command line in a terminal.

## Path

You can install Stack by copying the executable file anywhere on your PATH. A
good place to install is the same directory where Stack itself will install
executables, which depends on the operating system:

=== "Unix-like"

    Stack installs executables to:

    ~~~text
    $HOME/.local/bin
    ~~~

    If you don't have that directory in your PATH, you may need to update your
    PATH. That can be done by editing the `~/.bashrc` file.

=== "Windows"

    Stack installs executables to:

    ~~~text
    %APPDATA%\local\bin
    ~~~

    For example: `C:\Users\<user-name>\AppData\Roaming\local\bin`.

    If you don't have that directory in your PATH, you may need to update your
    PATH. That can be done by searching for 'Edit Environment variables for your
    account' under Start.

!!! note

    If you used [GHCup](https://www.haskell.org/ghcup/) to install Stack, GHCup
    puts executable files in the `bin` directory in the GHCup root directory.

## China-based users

If you're attempting to install Stack from within China:

* As of 24 February 2020, the download link has limited connectivity from within
  mainland China. If this is the case, please proceed by manually downloading
  (ideally via a VPN) and installing Stack per the instructions found on this
  page pertinent to your operating system.

* After installation, your `config.yaml` file will need to be configured before
  Stack can download large files consistently from within China (without
  reliance on a VPN). Please add the following to the bottom of the
  `config.yaml` file:

~~~yaml
###ADD THIS IF YOU LIVE IN CHINA
setup-info-locations:
- "http://mirrors.tuna.tsinghua.edu.cn/stackage/stack-setup.yaml"
urls:
  latest-snapshot: http://mirrors.tuna.tsinghua.edu.cn/stackage/snapshots.json

package-indices:
- download-prefix: http://mirrors.tuna.tsinghua.edu.cn/hackage/
~~~

## Using an HTTP proxy

To use Stack behind a HTTP proxy with IP address *IP* and port *PORT*, first set
up an environment variable `http_proxy` and then run the Stack command. For
example:

=== "Unix-like"

    ~~~text
    export http_proxy=IP:PORT
    stack install
    ~~~

    On most operating systems, it is not mandatory for programs to follow the
    "system-wide" HTTP proxy. Some programs, such as browsers, do honor this
    "system-wide" HTTP proxy setting, while other programs, including Bash, do
    not. That means configuring "http proxy setting" in your System Preferences
    (macOS) would not result in Stack traffic going through the proxy.

=== "Windows"

    ~~~text
    $Env:http_proxy=IP:PORT
    stack install
    ~~~

    It is not mandatory for programs to follow the "system-wide" HTTP proxy.
    Some programs, such as browsers, do honor this "system-wide" HTTP proxy
    setting, while other programs do not. That means configuring
    "http proxy setting" in your Control Panel would not result in Stack traffic
    going through the proxy.

## Upgrade Stack

There are different approaches to upgrading Stack, which vary as between
Unix-like operating systems (including macOS) and Windows.

!!! note

    If you used [GHCup](https://www.haskell.org/ghcup/) to install Stack, you
    should also use GHCup to upgrade Stack. GHCup uses an executable named
    `stack` to manage versions of Stack, through a file `stack.shim`. Stack will
    likely overwrite the executable on upgrade.

=== "Unix-like"

    There are essentially four different approaches:

    1.  The `stack upgrade` command, which downloads a Stack executable, or
        builds it from source, and installs it to Stack's 'local-bin' directory
        (see `stack path --local-bin`). If different and permitted, it also
        installs a copy in the directory of the current Stack executable. (If
        copying is not permitted, copy `stack` from Stack's 'local-bin'
        directory to the system location afterward.) You can use `stack upgrade`
        to get the latest official release, and `stack upgrade --git` to install
        from GitHub and live on the bleeding edge. Make sure the location of the
        Stack executable is on the PATH. See the [Path](#Path) section above.

    2.  If you're using a package manager and are happy with sticking with the
        officially released binaries from the distribution (which may the lag
        behind the latest version of Stack significantly), simply follow your
        normal package manager strategies for upgrading. For example:

        ~~~text
        apt-get update
        apt-get upgrade
        ~~~

    3.  The `get.haskellstack.org` script supports the `-f` argument to
        over-write the current Stack executable. For example, command:

        ~~~text
        curl -sSL https://get.haskellstack.org/ | sh -s - -f
        ~~~

        or:

        ~~~text
        wget -qO- https://get.haskellstack.org/ | sh -s - -f
        ~~~

    4.  Manually follow the steps above to download the newest executable from
        the GitHub releases page and replace the old executable.

=== "Windows"

    There are essentially two different approaches:

    1.  The `stack upgrade` command, which downloads a Stack executable, or
        builds it from source, and installs it to Stack's 'local-bin' directory
        (see `stack path --local-bin`). If different and permitted, it also
        installs a copy in the directory of the current Stack executable. (If
        copying is not permitted, copy `stack` from Stack's 'local-bin'
        directory to the system location afterward.) You can use `stack upgrade`
        to get the latest official release, and `stack upgrade --git` to install
        from GitHub and live on the bleeding edge. Make sure the location of the
        Stack executable is on the PATH. See the [Path](#Path) section above.

    2.  Manually follow the steps above to download the newest executable from
        the GitHub releases page and replace the old executable.

## Install earlier versions

To install a specific version of Stack, navigate to the desired version on the
[GitHub release page](https://github.com/commercialhaskell/stack/releases), and
click the appropriate link under its "Assets" drop-down menu.

Alternatively, use the URL
`https://github.com/commercialhaskell/stack/releases/download/vVERSION/stack-VERSION-PLATFORM.EXTENSION`.
For example, the tarball for Stack version 2.1.0.1, osx-x86_64 is at
`https://github.com/commercialhaskell/stack/releases/download/v2.1.0.1/stack-2.1.0.1-osx-x86_64.tar.gz`.

Here's a snippet for `appveyor.yml` files, borrowed from `dhall`'s
[`appveyor.yml`](https://github.com/dhall-lang/dhall-haskell/blob/1079b7a3a7a6922f72a373e47daf6f1b74f128b1/appveyor.yml).
Change the values of PATH and VERSION as needed.

~~~yaml
install:
  - set PATH=C:\Program Files\Git\mingw64\bin;%PATH%
  - curl --silent --show-error --output stack.zip --location "https://github.com/commercialhaskell/stack/releases/download/v%STACK_VERSION%/stack-%STACK_VERSION%-windows-x86_64.zip"
  - 7z x stack.zip stack.exe
  - stack setup > nul
  - git submodule update --init --recursive
~~~
