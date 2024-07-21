<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Install or upgrade

## Install Stack

Stack can be installed on most Linux distributions, macOS and Windows. It will
require at least about 5 GB of disk space, of which about 3 GB is for a single
version of GHC and about 2 GB is for Stack's local copy of the Hackage package
index.

Stack is open to supporting more operating systems. To request support for an
operating system, please submit an
[issue](https://github.com/commercialhaskell/stack/issues/new) at Stack's
GitHub repository.

Stack can be installed directly or by using the GHCup tool.

=== "Directly"

    Stack can be installed on various operating systems.

    !!! info "Releases on GitHub"

        Stack executables are also available on the
        [releases](https://github.com/commercialhaskell/stack/releases) page of
        Stack's GitHub repository.

    !!! info "`https://get.haskellstack.org/stable` URLs"

        URLs with the format
        `https://get.haskellstack.org/stable/<PLATFORM>.<EXTENSION>` point to
        the latest stable release. See the manual download links for examples.

    !!! info "`sh` script flags and options"

        The `sh` installation script recognises the following optional flags and
        options: `-q` suppresses output and specifies non-intervention (likely a
        prerequisite for the use of the script in CI environments); `-f` forces
        installation, even if an existing Stack executable is detected; and
        `-d <directory>` specifies a destination directory for the Stack
        executable.

    === "Linux"

        For most Linux distributions, on x86_64 or AArch64 machine
        architectures, the easiest way to install Stack is to command either:

        ~~~text
        curl -sSL https://get.haskellstack.org/ | sh
        ~~~

        or:

        ~~~text
        wget -qO- https://get.haskellstack.org/ | sh
        ~~~

        These commands download a script file and run it using `sh`.

        !!! note

            The script at [get.haskellstack.org](https://get.haskellstack.org/)
            will ask for root access using `sudo`. It needs such access in order
            to use your platform's package manager to install dependencies and
            to install to `/usr/local/bin`. If you prefer more control, follow
            the manual installation instructions for your platform below.

        ### Manual download

        Manual download for Linux distributions depends on your machine
        architecture, x86_64 or AArch64/ARM64.

        === "x86_64"

            * Click
              [:material-cloud-download-outline:](https://get.haskellstack.org/stable/linux-x86_64.tar.gz)
              to download an archive file with the latest release.

            * Extract the archive and place the `stack` executable somewhere on
              your PATH (see the [Path](#path) section below).

            * Ensure you have the required system dependencies installed. These
              include GCC, GNU Make, xz, perl, libgmp, libffi, and zlib. We also
              recommend Git and GPG.

            The installation of system dependencies will depend on the package
            manager for your Linux distribution. Notes are provided for Arch
            Linux, CentOS, Debian, Fedora, Gentoo and Ubuntu.

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

                Ensure you have the `ncurses` package with `USE=tinfo`. Without
                it, Stack will not be able to install GHC.

            === "Ubuntu"

                ~~~text
                sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase
                ~~~

        === "AArch64"

            * Click
              [:material-cloud-download-outline:](https://get.haskellstack.org/stable/linux-aarch64.tar.gz)
              to download an archive file with the latest release.

            * Extract the archive and place the `stack` executable somewhere on
              your PATH (see the [Path](#path) section below).

            * Ensure you have the required system dependencies installed. These
              include GCC, GNU Make, xz, perl, libgmp, libffi, and zlib. We also
              recommend Git and GPG.

            The installation of system dependencies will depend on the package
            manager for your Linux distribution. Notes are provided for Arch
            Linux, CentOS, Debian, Fedora, Gentoo and Ubuntu.

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
        including Arch Linux, Debian, Fedora, NixOS, openSUSE/SUSE Linux
        Enterprise, and Ubuntu. However, the Stack version available as a Linux
        package may lag behind Stack's current version and, in some cases, the
        lag may be significant.

        !!! info "Linux packages that lag behind Stack's current version"

            If Stack version available as a Linux package lags behind Stack's
            current version, using `stack upgrade --binary-only` is recommended
            after installing it.

        === "Arch Linux"

            The Arch extra package repository provides an official x86_64
            [package](https://www.archlinux.org/packages/extra/x86_64/stack/).
            You can install it with the command:

            ~~~text
            sudo pacman -S stack
            ~~~

            The Arch User Repository (AUR) also provides:

            *   a [`stack-bin` package](https://aur.archlinux.org/packages/stack-bin);
                and

            *   a [`stack-static` package](https://aur.archlinux.org/packages/stack-static)

        === "Debian"

            There are Debian
            [packages](https://packages.debian.org/search?keywords=haskell-stack&searchon=names&suite=all&section=all)
            for Buster and up. However, the distribution's Stack version lags
            behind.

        === "Fedora"

            Fedora includes Stack, but its Stack version may lag behind.

        === "NixOS"

            Users who follow the `nixos-unstable` channel or the Nixpkgs
            `master` branch can install the latest Stack release into their
            profile with the command:

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

            For more information on using Stack together with Nix, please see
            the
            [NixOS manual section on Stack](http://nixos.org/nixpkgs/manual/#how-to-build-a-haskell-project-using-stack).

        === "SUSE"

            There is also an unofficial package for openSUSE or SUSE Linux
            Enterprise. Its Stack version may lag behind. To install it:

            === "openSUSE Tumbleweed"

                ~~~text
                sudo zypper in stack
                ~~~

            === "openSUSE Leap"

                ~~~text
                sudo zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/openSUSE_Leap_42.1/devel:languages:haskell.repo
                sudo zypper in stack
                ~~~

            === "SUSE Linux Enterprise 12"

                ~~~text
                sudo zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/SLE_12/devel:languages:haskell.repo
                sudo zypper in stack
                ~~~

        === "Ubuntu"

            There are Ubuntu
            [packages](http://packages.ubuntu.com/search?keywords=haskell-stack&searchon=names&suite=all&section=all)
            for Ubuntu 20.04 and up.

        It is possible to set up auto-completion of Stack commands. For further
        information, see the [shell auto-completion](shell_autocompletion.md)
        documentation.

    === "macOS"

        Most users of Stack on macOS will also have up to date tools for software
        development (see [Xcode Command Line Tools](#xcode-command-line-tools)
        below).

        From late 2020, Apple began a transition from Mac computers with Intel
        processors (Intel-based Mac) to
        [Mac computers with Apple silicon](https://support.apple.com/en-gb/HT211814).

        === "Intel-based"

            Intel-based Mac computers have processors with x86_64 architectures.
            For most Intel-based Mac computers, the easiest way to install Stack
            is to command either:

            ~~~text
            curl -sSL https://get.haskellstack.org/ | sh
            ~~~

            or:

            ~~~text
            wget -qO- https://get.haskellstack.org/ | sh
            ~~~

            These commands download a script file and run it using `sh`.

            !!! note

                The script at
                [get.haskellstack.org](https://get.haskellstack.org/)
                will ask for root access using `sudo`. It needs such access in
                order to use your platform's package manager to install
                dependencies and to install to `/usr/local/bin`. If you prefer
                more control, follow the manual installation instructions below.

            !!! info

                We generally test on the current version of macOS and do our
                best to keep it compatible with the three most recent major
                versions. Stack may also work on older versions.

            ### Manual download

            * Click
              [:material-cloud-download-outline:](https://get.haskellstack.org/stable/osx-x86_64.tar.gz)
              to download an archive file with the latest release for x86_64
              architectures.

            * Extract the archive and place `stack` somewhere on your PATH (see the
              [Path](#path) section below).

            * Now you can run Stack from the command line in a terminal.

        === "Apple silicon"

            Mac computers with Apple silicon have an M series chip. These chips
            use an architecture known as ARM64 or AArch64.

            For Mac computers with Apple silicon, the easiest way to install
            Stack is to command either:

            ~~~text
            curl -sSL https://get.haskellstack.org/ | sh
            ~~~

            or:

            ~~~text
            wget -qO- https://get.haskellstack.org/ | sh
            ~~~

            These commands download a script file and run it using `sh`.

            !!! note

                The script at
                [get.haskellstack.org](https://get.haskellstack.org/)
                will ask for root access using `sudo`. It needs such access in
                order to use your platform's package manager to install
                dependencies and to install to `/usr/local/bin`. If you prefer
                more control, follow the manual installation instructions below.

            The installation of Stack or some packages (e.g. `network`)
            requiring C source compilation might fail with:

            ~~~text
            configure: error: C compiler cannot build executables
            ~~~

            In that case you should pass `-arch arm64` as part of the `CFLAGS`
            environment variable. This setting will be picked up by the C
            compiler of your choice.

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

            The setting instructs the C compiler to compile objects for ARM64.
            These can then be linked with libraries built for ARM64. Without the
            instruction, the C compiler, invoked by Cabal running in x86-64,
            would compile x86-64 objects and attempt to link them with existing
            ARM64 libraries, resulting in the error above.

            ### Manual download

            * Click
              [:material-cloud-download-outline:](https://get.haskellstack.org/stable/osx-aarch64.tar.gz)
              to download an archive file with the latest release for AArch64
              architectures.

            * Extract the archive and place `stack` somewhere on your PATH (see
              the [Path](#path) section below).

            * Now you can run Stack from the command line in a terminal.

            ### LLVM

            The documentation for each version of GHC identifies the versions of
            LLVM that are supported. That is summarised in the table below for
            recent versions of GHC:

            |GHC version|LLVM versions|
            |-----------|-------------|
            |9.8.2      |11 to 15     |
            |9.6.6      |11 to 15     |
            |9.4.8      |10 to 14     |
            |9.2.8      |9 to 12      |
            |9.0.2      |9, 10 or 12  |
            |8.10.7     |9 to 12      |
            |8.8.4      |7            |
            |8.6.5      |6            |
            |8.4.4      |5            |

        ### Using Homebrew

        [Homebrew](https://brew.sh/) is a popular package manager for macOS. If
        you have its `brew` tool installed, you can just command:

        ~~~text
        brew install haskell-stack
        ~~~

        * The Homebrew formula and bottles are **unofficial** and lag slightly
          behind new Stack releases, but tend to be updated within a day or two.

        * Normally, Homebrew will install from a pre-built binary (aka "pour
          from a bottle"), but if it starts trying to build everything from
          source (which will take hours), see
          [their FAQ on the topic](https://github.com/Homebrew/brew/blob/master/docs/FAQ.md#why-do-you-compile-everything).

        ### Xcode Command Line Tools

        macOS does not come with all the tools required for software development
        but a collection of useful tools, known as the Xcode Command Line Tools,
        is readily available. A version of that collection is provided with each
        version of Xcode (Apple’s integrated development environment) and can
        also be obtained from Apple separately from Xcode. The collection also
        includes the macOS SDK (software development kit). The macOS SDK
        provides header files for macOS APIs.

        If you use a command that refers to a common Xcode Command Line Tool and
        the Xcode Command Line Tools are not installed, macOS may prompt you to
        install the tools.

        macOS also comes with a command line tool, `xcode-select`, that can be
        used to obtain the Xcode Command Line Tools. Command
        `xcode-select --print-path` to print the path to the currently selected
        (active) developer directory. If the directory does not exist, or is
        empty, then the Xcode Command Line Tools are not installed.

        If the Xcode Command Line Tools are not installed, command
        `xcode-select --install` to open a user interface dialog to request
        automatic installation of the tools.

        An upgrade of macOS may sometimes require the existing Xcode Command
        Line Tools to be uninstalled and an updated version of the tools to be
        installed. The existing tools can be uninstalled by deleting the
        directory reported by `xcode-select --print-path`.

        If, after the installation of Stack, running `stack setup` fails with
        `configure: error: cannot run C compiled programs.` that indicates that
        the Xcode Command Line Tools are not installed.

        If building fails with messages that `*.h` files are not found, that may
        also indicate that Xcode Command Line Tools are not up to date.

        Xcode 10 provided an SDK for macOS 10.14 (Mojave) and
        [changed the location](https://developer.apple.com/documentation/xcode-release-notes/xcode-10-release-notes#Command-Line-Tools)
        of the macOS system headers. As a workaround, an extra package was
        provided by Apple which installed the headers to the base system under
        `/usr/include`.

        ### Auto-completion of Stack commands

        It is possible to set up auto-completion of Stack commands. For further
        information, see the [shell auto-completion](shell_autocompletion.md)
        documentation.

    === "Windows"

        Most machines using the Windows operating system have a x86_64
        architecture. More recently, Microsoft has provided Windows on Arm that
        runs on other processors.

        === "x86_64"

            On 64-bit Windows, the easiest way to install Stack is to download
            and use the
            [Windows installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

            !!! info "Stack root"

                By default, the Windows installer will set the
                [Stack root](topics/stack_root.md) by setting the `STACK_ROOT`
                environment variable to `C:\sr`.

            !!! note "Anti-virus software"

                Systems with antivirus software may need to add Stack to the
                list of 'trusted' applications.

                You may see a "Windows Defender SmartScreen prevented an
                unrecognized app from starting" warning when you try to run the
                installer. If so, click on **More info**, and then click on the
                **Run anyway** button that appears.

            !!! warning "Long user PATH environment variable"

                The Windows installer for Stack 2.9.1, 2.9.3 and 2.11.1 (only)
                will replace the user `PATH` environment variable (rather than
                append to it) if a 1024 character limit is exceeded. If the
                content of your existing user `PATH` is long, preserve it before
                running the installer.

            We recommend installing to the default location with the installer,
            as that will make `stack install` and `stack upgrade` work correctly
            out of the box.

            ### Manual download

            * Click
              [:material-cloud-download-outline:](https://get.haskellstack.org/stable/windows-x86_64.zip)
              to download an archive file with the latest release.

            * Unpack the archive and place `stack.exe` somewhere on your PATH
              (see the [Path](#path) section below).

            * Now you can run Stack from the command line in a terminal.

        === "Windows on Arm"

            The GHC project does not yet provide a version of GHC that runs on
            Windows on Arm.

    ## Path

    You can install Stack by copying the executable file anywhere on your PATH.
    A good place to install is the same directory where Stack itself will
    install executables, which depends on the operating system:

    === "Unix-like"

        Stack installs executables to:

        ~~~text
        $HOME/.local/bin
        ~~~

        If you don't have that directory in your PATH, you may need to update
        your PATH. That can be done by editing the `~/.bashrc` file.

    === "Windows"

        Stack installs executables to:

        ~~~text
        %APPDATA%\local\bin
        ~~~

        For example: `C:\Users\<user-name>\AppData\Roaming\local\bin`.

        If you don't have that directory in your PATH, you may need to update
        your PATH. That can be done by searching for 'Edit Environment variables
        for your account' under Start.

    !!! note

        If you used [GHCup](https://www.haskell.org/ghcup/) to install Stack,
        GHCup puts executable files in the `bin` directory in the GHCup root
        directory.

    !!! note "China-based users"

        As of 24 February 2020, the download link has limited connectivity from
        within mainland China. If you experience this, please proceed by
        manually downloading (ideally via a VPN) and installing Stack following
        the instructions on this page that apply to your operating system.

=== "GHCup"

    The separate [GHCup](https://www.haskell.org/ghcup/) project provides a tool
    that can be used to install Stack and other Haskell-related tools, including
    GHC and
    [Haskell Language Server](https://github.com/haskell/haskell-language-server)
    (HLS). HLS is a program that is used by Haskell extensions for popular code
    editors.

    GHCup provides Stack for some combinations of machine architecture and
    operating system not provided elsewhere.

    By default, the script to install GHCup (which can be run more than once)
    also configures Stack so that if Stack needs a version of GHC, GHCup takes
    over obtaining and installing that version.

## China-based users: configuration

After installation, your `config.yaml` file will need to be configured before
Stack can download large files consistently from within China (without reliance
on a VPN). Please add the following to the bottom of the `config.yaml` file:

~~~yaml
###ADD THIS IF YOU LIVE IN CHINA
setup-info-locations:
- "http://mirrors.tuna.tsinghua.edu.cn/stackage/stack-setup.yaml"
urls:
  latest-snapshot: http://mirrors.tuna.tsinghua.edu.cn/stackage/snapshots.json

package-index:
- download-prefix: http://mirrors.tuna.tsinghua.edu.cn/hackage/
~~~

## Using an HTTP proxy

To use Stack behind a HTTP proxy with IP address *IP* and port *PORT*, first
set up an environment variable `http_proxy` and then run the Stack command.
For example:

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

If Stack is already installed, upgrading it depends on whether you are using
Stack or GHCup to manage versions of Stack.

=== "Stack"

    There are different approaches to upgrading Stack, which vary as between
    Unix-like operating systems (including macOS) and Windows.

    !!! warning

        If you used [GHCup](https://www.haskell.org/ghcup/) to install Stack,
        you should also use GHCup to upgrade Stack. GHCup uses an executable
        named `stack` to manage versions of Stack, through a file `stack.shim`.
        Stack will likely overwrite the executable on upgrade.

    === "Unix-like"

        There are essentially four different approaches:

        1.  The `stack upgrade` command, which downloads a Stack executable, or
            builds it from source, and installs it to Stack's 'local-bin'
            directory (see `stack path --local-bin`). If different and
            permitted, it also installs a copy in the directory of the current
            Stack executable. (If copying is not permitted, copy `stack` from
            Stack's 'local-bin' directory to the system location afterward.)
            You can use `stack upgrade` to get the latest official release, and
            `stack upgrade --git` to install from GitHub and live on the
            bleeding edge. Make sure the location of the Stack executable is on
            the PATH. See the [Path](#Path) section above.

        2.  If you're using a package manager and are happy with sticking with
            the officially released binaries from the distribution (which may
            lag behind the latest version of Stack significantly), simply follow
            your normal package manager strategies for upgrading. For example:

            ~~~text
            apt-get update
            apt-get upgrade
            ~~~

        3.  The `get.haskellstack.org` script supports the `-f` argument to
            over-write the current Stack executable. For example, command
            either:

            ~~~text
            curl -sSL https://get.haskellstack.org/ | sh -s - -f
            ~~~

            or:

            ~~~text
            wget -qO- https://get.haskellstack.org/ | sh -s - -f
            ~~~

        4.  Manually follow the steps above to download the newest executable
            from the GitHub releases page and replace the old executable.

    === "Windows"

        There are essentially two different approaches:

        1.  The `stack upgrade` command, which downloads a Stack executable, or
            builds it from source, and installs it to Stack's 'local-bin'
            directory (see `stack path --local-bin`). If different and
            permitted, it also installs a copy in the directory of the current
            Stack executable. (If copying is not permitted, copy `stack` from
            Stack's 'local-bin' directory to the system location afterward.) You
            can use `stack upgrade` to get the latest official release, and
            `stack upgrade --git` to install from GitHub and live on the
            bleeding edge. Make sure the location of the Stack executable is on
            the PATH. See the [Path](#Path) section above.

        2.  Manually follow the steps above to download the newest executable
            from the GitHub releases page and replace the old executable.

=== "GHCup"

    The separate [GHCup](https://www.haskell.org/ghcup/) project provides
    guidance about how to use GHCup to manage versions of tools such as Stack.

## Install earlier versions

Installing earlier versions of Stack depends on whether you are using Stack or
GHCup to manage versions of Stack.

=== "Stack"

    To install a specific version of Stack, navigate to the desired version on
    the [GitHub release page](https://github.com/commercialhaskell/stack/releases),
    and click the appropriate link under its "Assets" drop-down menu.

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

=== "GHCup"

    The separate [GHCup](https://www.haskell.org/ghcup/) project provides
    guidance about how to use GHCup to manage versions of tools such as Stack.
