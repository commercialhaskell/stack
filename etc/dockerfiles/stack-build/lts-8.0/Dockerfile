FROM ubuntu:16.04

MAINTAINER Emanuel Borsboom <manny@fpcomplete.com>

ARG GHC_VERSION=8.0.2
ARG LTS_SLUG=lts-8.0
ARG PID1_VERSION=0.1.0.1
ARG DEBIAN_FRONTEND=noninteractive

#
# Set encoding to UTF-8 and PATH to find GHC and cabal/stack-installed binaries.
#

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/root/.cabal/bin:/root/.local/bin:/opt/ghc/$GHC_VERSION/bin:$PATH

#
# Use Stackage's debian-bootstrap.sh script to install system libraries and
# tools required to build any Stackage package.
#

#@@@ TEST documentation location
RUN apt-get update && \
    apt-get install -y wget && \
    wget -qO- https://raw.githubusercontent.com/fpco/stackage/93738c5865fdd0627508617215d01b78d4e2bf74/debian-bootstrap.sh | bash && \
    ln -s ghc /opt/ghc/$GHC_VERSION/share/doc/ghc-$GHC_VERSION && \
    rm -rf /var/lib/apt/lists/*

#
# Use 'stack' to install basic Haskell tools like alex, happy, and cpphs. We
# remove most of the STACK_ROOT afterward to save space, but keep the 'share'
# files that some of these tools require.
#

RUN stack --system-ghc --resolver=$LTS_SLUG --local-bin-path=/usr/local/bin install \
        cabal-install happy alex cpphs gtk2hs-buildtools hscolour && \
    cd $HOME/.stack && \
    find . -type f -not -path './snapshots/*/share/*' -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

#
# Install proper 'pid1' init daemon
#

RUN wget -O- "https://github.com/fpco/pid1/releases/download/pid1%2F$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" |tar xzf - -C /usr/local

#
# Set up pid1 entrypoint and default command
#

ENTRYPOINT ["/usr/local/sbin/pid1"]
CMD ["bash"]

#@@@: update stack docks to omit run and full images