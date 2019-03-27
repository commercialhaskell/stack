FROM ubuntu:16.04

MAINTAINER Emanuel Borsboom <manny@fpcomplete.com>

ARG GHC_VERSION=8.4.4
ARG LTS_SLUG=lts-12.0
ARG PID1_VERSION=0.1.2.0
ARG STACK_VERSION=1.9.3
ARG CUDA_VERSION=8.0
ARG BOOTSTRAP_COMMIT=7f982f63a3734ddb2a7cbbc52b8cec983c496efa
ARG DEBIAN_FRONTEND=noninteractive
ARG VARIANT=build

#
# Set encoding to UTF-8 and PATH to find GHC and cabal/stack-installed binaries.
#

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/root/.cabal/bin:/root/.local/bin:/usr/local/cuda-$CUDA_VERSION/bin:/opt/ghc/$GHC_VERSION/bin:$PATH \
    CUDA_PATH=/usr/local/cuda-$CUDA_VERSION \
    LD_LIBRARY_PATH=/usr/local/cuda-$CUDA_VERSION/lib64:/usr/local/cuda-$CUDA_VERSION/nvvm/lib64
#
# Use Stackage's debian-bootstrap.sh script to install system libraries and
# tools required to build any Stackage package.
#

RUN apt-get update && \
    apt-get install -y --no-install-recommends wget netbase ca-certificates && \
    if [ "$VARIANT" = "small" ]; then \
        echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main" >>/etc/apt/sources.list && \
        echo "deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main" >>/etc/apt/sources.list && \
        apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 063DAB2BDC0B3F9FCEBC378BFF3AEACEF6F88286 && \
        apt-get update && \
	    apt-get install -y --no-install-recommends \
	    	ghc-$GHC_VERSION ghc-$GHC_VERSION-htmldocs \
			g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg \
            libtinfo-dev; \
    else \
        wget -qO- https://raw.githubusercontent.com/fpco/stackage/$BOOTSTRAP_COMMIT/debian-bootstrap.sh | bash; \
    fi && \
    rm -rf /var/lib/apt/lists/*

#
# Create symlink to help tools find GHC documentation
#

RUN ln -s ghc /opt/ghc/$GHC_VERSION/share/doc/ghc-$GHC_VERSION

#
# Install Stack
#

RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'

#
# Configure Stack to use the GHC installed in the Docker image rather than installing its own
#

RUN mkdir /etc/stack/ && \
    echo "system-ghc: true" >/etc/stack/config.yaml

#
# Use 'stack' to install basic Haskell tools like alex, happy, and cpphs. We
# remove most of the STACK_ROOT afterward to save space, but keep the 'share'
# files that some of these tools require.
#

RUN export STACK_ROOT=/usr/local/lib/stack && \
    stack --resolver=$LTS_SLUG --local-bin-path=/usr/local/bin install \
        cabal-install happy alex cpphs gtk2hs-buildtools hscolour hlint hindent && \
    cd $STACK_ROOT && \
    find . -type f -not -path './snapshots/*/share/*' -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

#
# Install 'pid1' init daemon
#

RUN wget -O- "https://github.com/fpco/pid1/releases/download/v$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1

#
# Set up pid1 entrypoint and default command
#

ENTRYPOINT ["/usr/local/sbin/pid1"]
CMD ["bash"]
