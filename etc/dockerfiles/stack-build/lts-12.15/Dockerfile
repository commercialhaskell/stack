FROM fpco/stack-build:lts-12.0
ARG STACK_VERSION=1.9.1
ARG GHC_VERSION=8.4.4
RUN apt-get update && \
    apt-get install -y \
        ghc-$GHC_VERSION \
        ghc-$GHC_VERSION-dyn \
        ghc-$GHC_VERSION-htmldocs \
        ghc-$GHC_VERSION-prof && \
    rm -rf /var/lib/apt/lists/*
RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
RUN ln -s ghc-$GHC_VERSION /opt/ghc/$GHC_VERSION/share/doc/ghc
ARG CUDA_VERSION=8.0
ENV PATH=/root/.cabal/bin:/root/.local/bin:/usr/local/cuda-$CUDA_VERSION/bin:/opt/ghc/$GHC_VERSION/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
