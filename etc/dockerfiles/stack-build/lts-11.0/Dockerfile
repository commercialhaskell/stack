FROM ubuntu:16.04

MAINTAINER Emanuel Borsboom <manny@fpcomplete.com>

ARG GHC_VERSION=8.2.2
ARG LTS_SLUG=lts-11.0
ARG PID1_VERSION=0.1.0.1
ARG STACK_VERSION=1.6.5
ARG BOOTSTRAP_COMMIT=13ab2b86779c98598e96af7f4c4b9653ba280be5
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

RUN apt-get update && \
    apt-get install -y wget && \
    wget -qO- https://raw.githubusercontent.com/fpco/stackage/$BOOTSTRAP_COMMIT/debian-bootstrap.sh | bash && \
    rm -rf /var/lib/apt/lists/*

#
# Create symlink to help tools find GHC documentation
#

RUN ln -s ghc /opt/ghc/$GHC_VERSION/share/doc/ghc-$GHC_VERSION

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
# Install 'pid1' init daemon
#

RUN wget -O- "https://github.com/fpco/pid1/releases/download/pid1%2F$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1

#
# Install Stack
#

RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'

#
# Set up pid1 entrypoint and default command
#

ENTRYPOINT ["/usr/local/sbin/pid1"]
CMD ["bash"]
