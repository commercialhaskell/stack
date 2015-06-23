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

## Ubuntu

* Ubuntu 15.04 (amd64)

```sh
wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 14.10 (amd64)

```sh
wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/utopic stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 14.04 (amd64)

```sh
wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 12.04 (amd64)

```sh
wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```
## Arch Linux

stack can be found in the AUR:
  - [haskell-stack](https://aur4.archlinux.org/packages/haskell-stack/) _latest stable version_
  - [haskell-stack-git](https://aur4.archlinux.org/packages/haskell-stack-git/) _git version_

In order to install stack from Hackage or from source, you will need the [libtinfo](https://aur4.archlinux.org/packages/libtinfo/) Arch Linux package installed.  If this package is not installed, stack will not be able to install GHC.  

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

Once you have stack (version 0.0.2 or later), you can upgrade by running `stack update && stack install stack:latest`.