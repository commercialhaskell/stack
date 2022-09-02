FROM ubuntu:20.04

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y \
    curl build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libnuma-dev xz-utils \
    g++ gcc libc6-dev libffi-dev libgmp-dev make zlib1g-dev git gnupg netbase

RUN cd /tmp && \
    curl -L https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/clang+llvm-9.0.1-aarch64-linux-gnu.tar.xz --output /tmp/llvm.tar.xz && \
    unxz /tmp/llvm.tar.xz && \
    tar xfv /tmp/llvm.tar --strip-components 1 -C /usr && \
    rm /tmp/llvm.tar

RUN curl -sSL https://github.com/commercialhaskell/stack/releases/download/v2.7.1/stack-2.7.1-linux-aarch64.bin > /usr/local/bin/stack && \
    chmod +x /usr/local/bin/stack

ARG USERID
ARG GROUPID

RUN useradd --uid $USERID stack
RUN mkdir -p /home/stack
RUN chown -R stack /home/stack
RUN usermod -aG $GROUPID stack

USER stack
WORKDIR /home/stack

COPY stack.yaml package.yaml /src/

USER root

RUN chown -R stack /src

USER stack
WORKDIR /src

RUN stack build --only-snapshot --test
RUN stack build shake

COPY etc/scripts/release.hs /src

RUN stack script --resolver nightly-2022-08-02 --extra-dep Cabal-3.6.3.0 --compile /src/release.hs -- --version
RUN cp /src/release /home/stack
