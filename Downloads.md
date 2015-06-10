Distribution packages are available for [Ubuntu](#ubuntu) and [Arch Linux](#arch-linux). Binaries for other operating systems are available on [the releases page](https://github.com/fpco/stack/releases). For the future, we have plans to support more OSes.

## Windows

(32-bit)

* Download [stack-0.0.0-i386-windows.zip](https://github.com/fpco/stack/releases/download/v0.0.0-beta/stack-0.0.0-i386-windows.zip)
* Unpack the `stack.exe` to somewhere on your `%PATH%` and you can then run `stack` on the commandline.

## OS X

* Download [stack-0.0.0-x86_64-osx.gz](https://github.com/fpco/stack/releases/download/v0.0.0-beta/stack-0.0.0-x86_64-osx.gz)
* Extract the `stack` executable and put it somewhere on your `$PATH` (e.g. `/usr/local/bin`)
* **Note**: due to [GHC bug 10322](https://ghc.haskell.org/trac/ghc/ticket/10322), `stack` does not build with GHC 7.10.1. This bug will be fixed in 7.10.2, but in the meantime, we recommend using GHC 7.8.4 on OS X.

## Ubuntu

* Ubuntu 15.04 (amd64)

```sh
wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://downloads.fpcomplete.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
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

## Linux

(64-bit only)

* Download [stack-0.0.0-x86_64-linux.gz](https://github.com/fpco/stack/releases/download/v0.0.0-beta/stack-0.0.0-x86_64-linux.gz)
* Extract the `stack` executable into `/usr/local/bin`.
* Now you can run `stack` from the terminal.