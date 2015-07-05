Distribution packages are available for [Ubuntu](#ubuntu) and [Arch Linux](#arch-linux). Binaries for other operating systems are available on [the releases page](https://github.com/fpco/stack/releases). For the future, we have plans to support more OSes.

## Windows

(32-bit)

* Download [the latest release](https://github.com/commercialhaskell/stack/releases/latest)
* Unpack the `stack.exe` to somewhere on your `%PATH%` (see [Path section below](#path)) and you can then run `stack` on the commandline.

NOTE: This executable has been built and tested on a Windows 8.1 64-bit machine. It should run on older Windows installs as well, but has not been tested. If you do test, please edit and update this page to indicate as such.

## OS X

* Download [the latest release](https://github.com/commercialhaskell/stack/releases/latest)
* Extract the `stack` executable and put it somewhere on your `$PATH` (see [Path section below](#path))

We generally test on the current version of OS X, but stack is known to work on Mavericks as well, and may also work on older versions (YMMV).

**Note**: due to [GHC bug 10322](https://ghc.haskell.org/trac/ghc/ticket/10322), building `stack` from source fails with GHC 7.10.1. This bug will be fixed in 7.10.2, but in the meantime, we recommend using GHC 7.8.4 on OS X.

**Note**: If you see the message `bash: <PATH_TO_STACK>/stack: Permission denied` or `stack isn't recoginzed` set the executable bit `chmod +x <PATH_TO_STACK>/stack`

## Ubuntu

1. Get the FP Complete key:

        wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -

2. Add the appropriate source repository:

    * Ubuntu 15.04 (amd64):

            echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Ubuntu 14.10 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu/utopic stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Ubuntu 14.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Ubuntu 12.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## Debian

1. Get the FP Complete key:

        wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/debian/fpco.key | sudo apt-key add -

2. Add the appropriate source repository:

    * Debian 8 (amd64):

            echo 'deb http://download.fpcomplete.com/debian/jessie stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Debian 7 (amd64)

            echo 'deb http://download.fpcomplete.com/debian/wheezy stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## CentOS / Red Hat

1. Add the appropriate source repository:

    * CentOS 7 / RHEL 7 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

2. Install:

        sudo yum -y install stack

## Fedora

1. Add the appropriate source repository:

    * Fedora 22 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/22/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

    * Fedora 21 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/21/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

    * Fedora 20 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/20/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

2. Install:

    * Fedora 22+

            sudo dnf -y install stack

    * Fedora < 22

            sudo yum -y install stack

## Arch Linux

stack can be found in the AUR:
  - [haskell-stack](https://aur.archlinux.org/packages/haskell-stack/) _latest stable version_
  - [haskell-stack-git](https://aur4.archlinux.org/packages/haskell-stack-git/) _git version_

In order to install stack from Hackage or from source, you will need the [libtinfo](https://aur4.archlinux.org/packages/libtinfo/) Arch Linux package installed.  If this package is not installed, stack will not be able to install GHC.  

If you use the [ArchHaskell repository](https://wiki.archlinux.org/index.php/ArchHaskell), you can also get the `haskell-stack` package from there.

## Linux

(64-bit only)

* Download [the latest release](https://github.com/commercialhaskell/stack/releases/latest)
* Extract the `stack` executable somewhere on your `$PATH`  (see [Path section below](#path))
* Now you can run `stack` from the terminal.

Tested on Fedora 20: make sure to install the following packages `sudo yum install perl make automake gcc gmp-devel`

## Path

You can install stack by copying it anywhere on your PATH environment variable, such as `/usr/local/bin`. However, it may be advantageous to place the stack executable in the same directory where stack itself will install executables, that way stack is able to upgrade itself. On Windows, that directory is `%APPDATA%\local\bin`, e.g. "c:\Users\Michael\AppData\Roaming\local\bin", and on other systems `$HOME/.local/bin`.

If you're curious about the choice of these paths, see [issue #153](https://github.com/commercialhaskell/stack/issues/153)

## Upgrade

We're still [working out the best recommendation](https://github.com/commercialhaskell/stack/issues/237) for upgrades. Options for now are:

* If you have a recent `stack` (post 0.1.0.0 release), you can try the experimental `stack upgrade` or `stack upgrade --git` command.
* Clone the Git repository and run `stack install` inside of it
* Anywhere after the 0.1.0.0 release: run `stack update && stack unpack stack`, change into the new directory, and run `stack install`. Note that you may need to run `stack setup`
* Use your package manager when supported
* Manually download new binaries from this page
* In some cases, you'll be able to run `stack update && stack install stack:latest`, but this is not guaranteed to work (yet).