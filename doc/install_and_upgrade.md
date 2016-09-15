# Install/upgrade

For many Un*x operating systems, all you need to do is run:

    curl -sSL https://get.haskellstack.org/ | sh

or:

    wget -qO- https://get.haskellstack.org/ | sh

Distribution packages are available for [Ubuntu](#ubuntu), [Debian](#debian),
[CentOS / Red Hat / Amazon Linux](#centos), [Fedora](#fedora),
[Arch Linux](#arch-linux) and unofficially [FreeBSD](#freebsd).
Binaries for other operating systems are listed below, and available on
[the Github releases page](https://github.com/fpco/stack/releases). For the
future, we are open to supporting more OSes (to request one, please
[submit an issue](https://github.com/commercialhaskell/stack/issues/new)).

Binary packages are signed with this [signing key](SIGNING_KEY.md).

If you are writing a script that needs to download the latest binary, you can
find links that always point to the latest bindists
[here](https://www.stackage.org/stack).

## Windows

*Note*: Due to specific Windows limitations,
 [some temporary workarounds](https://www.fpcomplete.com/blog/2015/08/stack-ghc-windows)
 may be required. It is strongly advised to set your `STACK_ROOT` environment
 variable similarly to your root (e.g., `set STACK_ROOT=c:\stack_root`) *before*
 running `stack`.

*Note:* while generally 32-bit GHC is better tested on Windows, there are
reports that recent versions of Windows only work with the 64-bit version of
Stack (see
[issue #393](https://github.com/commercialhaskell/stack/issues/393)).

### Installer

We recommend installing to the default location with these installers, as that
will make `stack install` and `stack upgrade` work correctly out of the box.

  * [Windows 32-bit Installer](https://www.stackage.org/stack/windows-i386-installer)
  * [Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer)

### Manual download

* Download the latest release:

    * [Windows 32-bit](https://www.stackage.org/stack/windows-i386)
    * [Windows 64-bit](https://www.stackage.org/stack/windows-x86_64)

* Unpack the archive and place `stack.exe` somewhere on your `%PATH%` (see
  [Path section below](#path)) and you can then run `stack` on the command line.

* Now you can run `stack` from the terminal.

NOTE: These executables have been built and tested on a Windows 7, 8.1, and 10
64-bit machines. They should run on older Windows installs as well, but have not
been tested. If you do test, please edit and update this page to indicate as
such.

## Mac OS X

### Using Homebrew

If you have a popular [brew](http://brew.sh/) tool installed, you can just do:

    brew install haskell-stack

* The Homebrew formula and bottles are **unofficial** and lag slightly behind new Stack releases,
but tend to be updated within a day or two.
* Normally, Homebrew will install from a pre-built binary (aka "pour from a
bottle"), but if `brew` starts trying to build everything from source (which
will take hours), see
[their FAQ on the topic](https://github.com/Homebrew/homebrew/blob/master/share/doc/homebrew/FAQ.md#why-do-you-compile-everything).

### Manual download

* Download the latest release:
    * [Mac OS X 64-bit](https://www.stackage.org/stack/osx-x86_64)
* Extract the archive and place `stack` somewhere on your `$PATH` (see
  [Path section below](#path))
* Now you can run `stack` from the terminal.

We generally test on the current version of Mac OS X, but stack is known to work on
Yosemite and Mavericks as well, and may also work on older versions (YMMV).

### Notes

After installation, running `stack setup` might fail with `configure: error: cannot run C compiled programs.` in which case you should run:

    xcode-select --install

If you are on OS X 10.11 ("El Capitan") and encounter either of these
problems, see the linked FAQ entries:

  * [GHC 7.8.4 fails with `/usr/bin/ar: permission denied`](faq.md#usr-bin-ar-permission-denied)
  * [DYLD_LIBRARY_PATH is ignored](faq.md#dyld-library-path-ignored)

## Ubuntu

*note*: for 32-bit, use the [generic Linux option](#linux)

 1. Get the FP Complete key:

        sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442

 2. Add the appropriate source repository (if not sure, run ``lsb_release -a`` to find out your Ubuntu version):

      * Ubuntu 16.04 (amd64):

            echo 'deb http://download.fpcomplete.com/ubuntu xenial main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 15.10 (amd64):

            echo 'deb http://download.fpcomplete.com/ubuntu wily main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 14.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 12.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu precise main'|sudo tee /etc/apt/sources.list.d/fpco.list

 3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## Debian

*note*: for 32-bit, use the [generic Linux option](#linux)

 1. Get the FP Complete key:

        sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442

 2. Add the appropriate source repository:

      * Debian 8 (amd64):

            echo 'deb http://download.fpcomplete.com/debian jessie main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Debian 7 (amd64)

            echo 'deb http://download.fpcomplete.com/debian wheezy main'|sudo tee /etc/apt/sources.list.d/fpco.list

    For unstable Debian distributions, the package from the most recent stable
    release will usually work. If it doesn't, please
    [report it](https://github.com/commercialhaskell/stack/issues/new).

 3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## <a name="centos"></a>CentOS / Red Hat / Amazon Linux

*note*: for 32-bit, use the [generic Linux option](#linux)

 1. Add the appropriate source repository:

      * CentOS 7 / RHEL 7 (x86_64)

            curl -sSL https://download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

      * CentOS 6 / RHEL 6 (x86_64)

            curl -sSL https://download.fpcomplete.com/centos/6/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

 2. Install:

        sudo yum -y install stack

## Fedora

*Note*: for 32-bit, you can use this
 [Fedora Copr repo](https://copr.fedoraproject.org/coprs/petersen/stack/) (not
 managed by the Stack release team, so not guaranteed to have the very latest
 version) which can be enabled with: `sudo dnf copr enable petersen/stack`

   1. Add the appropriate source repository:

      * Fedora 24 (x86_64)

            curl -sSL https://download.fpcomplete.com/fedora/24/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

      * Fedora 23 (x86_64)

            curl -sSL https://download.fpcomplete.com/fedora/23/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

      * Fedora 22 (x86_64)

            curl -sSL https://download.fpcomplete.com/fedora/22/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

   2. Install:

        sudo dnf -y install stack

## <a name="suse"></a>openSUSE / SUSE Linux Enterprise

*Note:* openSUSE's and SLE's `stack` package isn't managed by the Stack release
team, and since it is based on the version in Stackage LTS, and may lag new
releases by ten days or more.

 1. Add the appropriate OBS repository:

      * openSUSE Tumbleweed

        all needed is in distribution

      * openSUSE Leap

            sudo zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/openSUSE_Leap_42.1/devel:languages:haskell.repo

      * SUSE Linux Enterprise 12

            sudo zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/SLE_12/devel:languages:haskell.repo 

 2. Install:

        sudo zypper in stack

## Arch Linux

*Note:* `stack` package in the [community] repository isn't managed by the
Stack release team. Depending on the maintainer's availability, it can lag
new releases by some days.

  - [stack](https://www.archlinux.org/packages/community/x86_64/stack/) _latest stable version_
  - [haskell-stack-git](https://aur.archlinux.org/packages/haskell-stack-git/) _git version_

In order to use `stack setup` with older versions of GHC or on a 32-bit system,
you may need the
[ncurses5-compat-libs](https://aur.archlinux.org/packages/ncurses5-compat-libs/)
AUR package installed. If this package is not installed, Stack may not be able
to install older (< 7.10.3) or 32-bit GHC versions.

If you use the
[ArchHaskell repository](https://wiki.archlinux.org/index.php/ArchHaskell), you
can also get the `haskell-stack-tool` package from there.

## NixOS

Users who follow the `nixos-unstable` channel or the Nixpkgs `master` branch can install the latest `stack` release into their profile by running:

    nix-env -f "<nixpkgs>" -iA haskellPackages.stack

Alternatively, the package can be built from source as follows.

 1. Clone the git repo:

        git clone https://github.com/commercialhaskell/stack.git

 2. Create a `shell.nix` file:

        cabal2nix --shell ./. --no-check --no-haddock > shell.nix

    Note that the tests fail on NixOS, so disable them with `--no-check`. Also, haddock currently doesn't work for stack, so `--no-haddock` disables it.

 3. Install stack to your user profile:

        nix-env -i -f shell.nix

For more information on using Stack together with Nix, please see [the NixOS
manual section on
Stack](http://nixos.org/nixpkgs/manual/#how-to-build-a-haskell-project-using-stack).

## Linux

(64-bit and 32-bit options available)

* Download the latest release:

    * [Linux 64-bit, standard](https://www.stackage.org/stack/linux-x86_64)
    * [Linux 32-bit, standard](https://www.stackage.org/stack/linux-i386)

    If you are on an older distribution that only includes libgmp4 (libgmp.so.3), such as CentOS/RHEL/Amazon Linux 6.x, use one of these instead:

    * [Linux 64-bit, libgmp4](https://www.stackage.org/stack/linux-x86_64-gmp4)
    * [Linux 32-bit, libgmp4](https://www.stackage.org/stack/linux-i386-gmp4)

* Extract the archive and place `stack` somewhere on your `$PATH` (see [Path section below](#path))

* Ensure you have required system dependencies installed.  These include GCC, GNU make, xz, perl, libgmp, libffi, and zlib.  We also recommend Git and GPG.  To install these using your package manager:
    * Debian / Ubuntu: `sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg`
    * Fedora / CentOS: `sudo dnf install perl make automake gcc gmp-devel libffi zlib xz tar git gnupg` (use `yum` instead of `dnf` on CentOS and Fedora <= 21)
        * Fedora 24: In order to use `stack setup` on a 32-bit system, you may
          need to run `sudo dnf install ncurses-compat-libs`. If this package is
          not installed, Stack may not be able to install 32-bit GHC versions.
        Also `sudo dnf install ncurses-compat-libs` if you nee
    * Arch Linux: `sudo pacman -S make gcc ncurses git gnupg xz zlib gmp libffi zlib`

        * In order to use `stack setup` with older versions of GHC or on a
          32-bit system, you may need the
          [ncurses5-compat-libs](https://aur.archlinux.org/packages/ncurses5-compat-libs/)
          AUR package installed. If this package is not installed, Stack may not
          be able to install older (< 7.10.3) or 32-bit GHC versions.
    * Gentoo users, make sure to have the `ncurses` package with `USE=tinfo` (without it, stack will not be able to install GHC).

* Now you can run `stack` from the terminal.

## FreeBSD

(only 64-bit currently available, tested on FreeBSD 10.3-RELEASE)

* Install required dependencies:

        pkg install devel/gmake perl5 lang/gcc misc/compat8x misc/compat9x converters/libiconv ca_root_nss

* Download the latest release:

    * [FreeBSD 64-bit](https://www.stackage.org/stack/freebsd-x86_64)

* Extract the archive and place `stack` somewhere on your `$PATH` (see [Path section below](#path))

* Now you can run `stack` from the terminal.

An unofficial package repository for FreeBSD 10 (amd64 only) and install
instructions are available at [http://stack-pkg.applicative.tech](http://stack-pkg.applicative.tech/).  The
repository is not official and as such might lag behind new releases.

## Path

You can install stack by copying it anywhere on your PATH environment variable. We recommend installing in the same directory where stack itself will install executables (that way stack is able to upgrade itself!). On Windows, that directory is `%APPDATA%\local\bin`, e.g. "c:\Users\Michael\AppData\Roaming\local\bin". For other systems, use `$HOME/.local/bin`.

If you don't have that directory in your PATH, you may need to update your PATH (such as by editing .bashrc).

If you're curious about the choice of these paths, see [issue #153](https://github.com/commercialhaskell/stack/issues/153)

## Shell auto-completion

To get tab-completion of commands on bash, just run the following (or add it to
`.bashrc`):

    eval "$(stack --bash-completion-script stack)"

For more information and other shells, see [the shell auto-completion page](shell_autocompletion.md)

## Upgrade

There are essentially three different approaches to upgrade:

* If you're using a package manager (e.g., the Ubuntu debs listed above) and are happy with sticking with the officially released binaries, simply follow your normal package manager strategies for upgrading (e.g. `apt-get update && apt-get upgrade`).
* If you're not using a package manager but want to stick with the official binaries (such as on Windows or Mac), you'll need to manually follow the steps above to download the newest binaries from the release page and replace the old binary.
* The `stack` tool itself ships with an `upgrade` command, which will build `stack` from source and install it to the default install path (see the previous section). You can use `stack upgrade` to get the latest official release, and `stack upgrade --git` to install from Git and live on the bleeding edge. If you follow this, make sure that this directory is on your `PATH` and takes precedence over the system installed `stack`. For more information, see [this discussion](https://github.com/commercialhaskell/stack/issues/237#issuecomment-126793301).
