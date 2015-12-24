# Install/upgrade

Distribution packages are available for [Ubuntu](#ubuntu), [Debian](#debian),
[CentOS / Red Hat / Amazon Linux](#centos-red-hat-amazon-linux), [Fedora](#fedora) and
[Arch Linux](#arch-linux). Binaries for other operating systems are listed
below, and available on
[the Github releases page](https://github.com/fpco/stack/releases). For the
future, we are open to supporting more OSes (to request one, please
[submit an issue](https://github.com/commercialhaskell/stack/issues/new)).

Binary packages are signed with this [signing key](SIGNING_KEY.html).

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

Note: if you are on OS X 10.11 ("El Capitan") or later, System Integrity
Protection (a.k.a. "rootless") can cause two problems:

  * [GHC 7.8.4 fails with `/usr/bin/ar: permission denied`](faq.html#usr-bin-ar-permission-denied)
  * [DYLD_LIBRARY_PATH is ignored](faq.html#dyld-library-path-ignored)

See the above FAQ links for workarounds.

### Using Homebrew

If you have a popular [brew](http://brew.sh/) tool installed, you can just do:

```
brew install haskell-stack
```

Note: the Homebrew formula and bottles lag slightly behind new Stack releases,
but tend to be updated within a day or two.

### Manual download

* Download the latest release:
    * [Mac OS X 64-bit](https://www.stackage.org/stack/osx-x86_64)
* Extract the archive and place `stack` somewhere on your `$PATH` (see
  [Path section below](#path))
* Now you can run `stack` from the terminal.

We generally test on the current version of Mac OS X, but stack is known to work on
Yosemite and Mavericks as well, and may also work on older versions (YMMV).

## Ubuntu

*note*: for 32-bit, use the [generic Linux option](#linux)

 1. Get the FP Complete key:

        sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442

 2. Add the appropriate source repository (if not sure, run ``lsb_release -a`` to find out your Ubuntu version):

      * Ubuntu 15.10 (amd64):

            echo 'deb http://download.fpcomplete.com/ubuntu wily main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 15.04 (amd64):

            echo 'deb http://download.fpcomplete.com/ubuntu vivid main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 14.10 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu utopic main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 14.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Ubuntu 12.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu precise main'|sudo tee /etc/apt/sources.list.d/fpco.list

 3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## Debian

*note*: for 32-bit, use the [generic Linux option](#linux)

 1. Get the FP Complete key:

        sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442

 2. Add the appropriate source repository:

      * Debian 8 (amd64):

            echo 'deb http://download.fpcomplete.com/debian jessie main'|sudo tee /etc/apt/sources.list.d/fpco.list

      * Debian 7 (amd64)

            echo 'deb http://download.fpcomplete.com/debian wheezy main'|sudo tee /etc/apt/sources.list.d/fpco.list

        Note: The official GHC >7.10.3 bindists do not support Debian 7, so
        `stack setup` will not work on Debian 7 with GHC >7.10.3.  We will drop
        support for Debian 7 once GHC 8.0 is released.

    For unstable Debian distributions, the package from the most recent stable
    release will usually work. If it doesn't, please
    [report it](https://github.com/commercialhaskell/stack/issues/new).

 3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## CentOS / Red Hat / Amazon Linux

*note*: for 32-bit, use the [generic Linux option](#linux)

 1. Add the appropriate source repository:

      * CentOS 7 / RHEL 7 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

      * CentOS 6 / RHEL 6 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/6/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

 2. Install:

        sudo yum -y install stack

## Fedora

*Note*: for 32-bit, you can use this
 [Fedora Copr repo](https://copr.fedoraproject.org/coprs/petersen/stack/) (not
 managed by the Stack release team, so not guaranteed to have the very latest
 version) which can be enabled with:

    sudo dnf copr enable petersen/stack

 1. Add the appropriate source repository:

      * Fedora 23 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/23/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

      * Fedora 22 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/22/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

      * Fedora 21 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/21/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

 2. Install:

      * Fedora 22 and above

            sudo dnf -y install stack

      * Fedora < 22

            sudo yum -y install stack

## openSUSE / SUSE Linux Enterprise

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

*note*: for 32-bit, use the [generic Linux option](#linux). (You will need to ensure libtinfo is installed, see below.)

stack can be found in the AUR:
  - [haskell-stack](https://aur.archlinux.org/packages/haskell-stack/) _latest stable version_
  - [haskell-stack-git](https://aur.archlinux.org/packages/haskell-stack-git/) _git version_

In order to install stack from Hackage or from source, you will need the [libtinfo](https://aur.archlinux.org/packages/libtinfo/) Arch Linux package installed.  If this package is not installed, stack will not be able to install GHC.  

If you use the [ArchHaskell repository](https://wiki.archlinux.org/index.php/ArchHaskell), you can also get the `haskell-stack` package from there.

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
Stack](http://nixos.org/nixpkgs/manual/#using-stack-together-with-nix).

## Linux

(64-bit and 32-bit options available)

* Download the latest release:

    * [Linux 64-bit, standard](https://www.stackage.org/stack/linux-x86_64)
    * [Linux 32-bit, standard](https://www.stackage.org/stack/linux-i386)

    If you are on an older distribution that only includes libgmp4 (libgmp.so.3), such as CentOS/RHEL/Amazon Linux 6.x, use one of these instead:

    * [Linux 64-bit, libgmp4](https://www.stackage.org/stack/linux-x86_64-gmp4)
    * [Linux 32-bit, libgmp4](https://www.stackage.org/stack/linux-i386-gmp4)

* Extract the archive and place `stack` somewhere on your `$PATH` (see [Path section below](#path))

* Now you can run `stack` from the terminal.

Tested on Fedora 20: make sure to install the following packages `sudo yum install perl make automake gcc gmp-devel`.
For Gentoo users, make sure to have the `ncurses` package with `USE=tinfo` (without it, stack will not be able to install GHC).

## Path

You can install stack by copying it anywhere on your PATH environment variable. We recommend installing in the same directory where stack itself will install executables (that way stack is able to upgrade itself!). On Windows, that directory is `%APPDATA%\local\bin`, e.g. "c:\Users\Michael\AppData\Roaming\local\bin". For other systems, use `$HOME/.local/bin`.

If you don't have that directory in your PATH, you may need to update your PATH (such as by editing .bashrc).

If you're curious about the choice of these paths, see [issue #153](https://github.com/commercialhaskell/stack/issues/153)

## Shell auto-completion

To get tab-completion of commands on bash, just run the following (or add it to
`.bashrc`):

    eval "$(stack --bash-completion-script stack)"

For more information and other shells, see [the shell auto-completion wiki
page](https://github.com/commercialhaskell/stack/wiki/Shell-autocompletion)

## Upgrade

There are essentially three different approaches to upgrade:

* If you're using a package manager (e.g., the Ubuntu debs listed above) and are happy with sticking with the officially released binaries, simply follow your normal package manager strategies for upgrading (e.g. `apt-get update && apt-get upgrade`).
* If you're not using a package manager but want to stick with the official binaries (such as on Windows or Mac), you'll need to manually follow the steps above to download the newest binaries from the release page and replace the old binary.
* The `stack` tool itself ships with an `upgrade` command, which will build `stack` from source and install it to the default install path (see the previous section). You can use `stack upgrade` to get the latest official release, and `stack upgrade --git` to install from Git and live on the bleeding edge. If you follow this, make sure that this directory is on your `PATH` and takes precedence over the system installed `stack`. For more information, see [this discussion](https://github.com/commercialhaskell/stack/issues/237#issuecomment-126793301).
