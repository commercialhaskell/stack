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

RUN useradd --uid $USERID stack && mkdir -p /home/stack && chown -R stack /home/stack && usermod -aG $GROUPID stack
USER stack
WORKDIR /home/stack

RUN stack setup ghc-8.10.4
RUN stack update

COPY stack.yaml package.yaml /src/
USER root
RUN chown -R stack /src
USER stack
RUN cd /src && stack build --only-snapshot --test && stack build shake

COPY etc/scripts/release.hs /src
RUN stack script --resolver lts-17.15 --compile /src/release.hs -- --version && cp /src/release /home/stack
