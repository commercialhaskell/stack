<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Adding a new GHC version

  * Push new tag to our fork:

        git clone git@github.com:commercialhaskell/ghc.git
        cd ghc
        git remote add upstream git@github.com:ghc/ghc.git
        git fetch upstream
        git push origin ghc-X.Y.Z-release

  * [Publish a new Github release](https://github.com/commercialhaskell/ghc/releases/new)
    with tag `ghc-X.Y.Z-release` and same name.

  * Download all the relevant GHC bindists from https://www.haskell.org/ghc/download_ghc_X_Y_Z and upload them to the just-created Github release (see
    [stack-setup-2.yaml](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml)
    for the ones we used in the last GHC release).

    In the case of macOS, repackage the `.xz` bindist as a `.bz2`, since macOS does
    not include `xz` by default or provide an easy way to install it.

    The script at `etc/scripts/mirror-ghc-bindists-to-github.sh` will help with
    this. See the comments within the script.

  * [Edit stack-setup-2.yaml](https://github.com/fpco/stackage-content/edit/master/stack/stack-setup-2.yaml)
    and add the new bindists, pointing to the Github release version. Be sure to
    update the `content-length` and `sha1` values.


## Building GHC

TODO: look into using https://github.com/bgamari/ghc-utils/blob/master/rel-eng/bin-release.sh, which is the script used to official bindists.

On systems with a small `/tmp`, you should set TMP and TEMP to an alternate
location.

Setup the system based on [these instructions](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Linux).  On Ubuntu (`docker run -ti --rm ubuntu:16.04`):

    apt-get update && apt-get install -y ghc alex happy make autoconf g++ git vim xz-utils automake libtool gcc libgmp-dev ncurses-dev libtinfo-dev python3

on Void Linux (`docker run -ti --rm voidlinux/voidlinux bash`):

    xbps-install -S curl gcc make xz ghc autoconf git vim automake gmp-devel ncurses-devel python3 cabal-install && \
    cabal update && \
    cabal install alex happy

For GHC >= 7.10.2, set the `GHC_VERSION` environment variable to the version to build:

  * `export GHC_VERSION=8.2.2`
  * `export GHC_VERSION=8.2.1`
  * `export GHC_VERSION=8.0.2`
  * `export GHC_VERSION=8.0.1`
  * `export GHC_VERSION=7.10.3a`
  * `export GHC_VERSION=7.10.2`

then, run (from [here](https://ghc.haskell.org/trac/ghc/wiki/Newcomers)):

    git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/ && \
    git clone -b ghc-${GHC_VERSION}-release --recursive git://github.com/ghc/ghc ghc-${GHC_VERSION} && \
    cd ghc-${GHC_VERSION}/ && \
    cp mk/build.mk.sample mk/build.mk && \
    sed -i 's/^#BuildFlavour *= *perf$/BuildFlavour = perf/' mk/build.mk && \
    ./boot && \
    ./configure --enable-tarballs-autodownload && \
    sed -i 's/^TAR_COMP *= *bzip2$/TAR_COMP = xz/' mk/config.mk && \
    make -j$(cat /proc/cpuinfo|grep processor|wc -l) && \
    make binary-dist

GHC 7.8.4 is slightly different:

    export GHC_VERSION=7.8.4 && \
    git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/ && \
    git clone -b ghc-${GHC_VERSION}-release --recursive git://github.com/ghc/ghc ghc-${GHC_VERSION} && \
    cd ghc-${GHC_VERSION}/ && \
    ./sync-all --extra --nofib -r git://git.haskell.org get -b ghc-7.8 && \
    cp mk/build.mk.sample mk/build.mk && \
    sed -i 's/^#BuildFlavour *= *perf$/BuildFlavour = perf/' mk/build.mk && \
    perl boot && \
    ./configure && \
    sed -i 's/^TAR_COMP *= *bzip2$/TAR_COMP = xz/' mk/config.mk && \
    make -j$(cat /proc/cpuinfo|grep processor|wc -l) && \
    make binary-dist
