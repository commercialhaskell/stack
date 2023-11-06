# This Dockerfile was previously used to build dynamically-linked Stack for
# Linux/Aarch64. It was used with the following step in the GitHub Actions CI:
#
#   run: |
#     set -ex
#     docker build . -f etc/dockerfiles/arm64.Dockerfile -t stack --build-arg USERID=$(id -u) --build-arg GROUPID=$(id -g)
#     rm -rf _release
#     mkdir -p _release
#     docker run --rm -v $(pwd):/src -w /src stack bash -c "/home/stack/release build"
#
# However, after Stack 2.11.1, it was replaced with a step that makes use of
# https://gitlab.com/benz0li/ghc-musl.
#
# ------------------------------------------------------------------------------
#
# Stack is built with GHC 9.2.8. GHC 9.2.8 for Linux/AArch64 says it was made on
# a Debian 10 system and requires GMP 6.1. Debian 10 is codename 'buster' and
# includes libc6 (2.28-10+deb10u1).
FROM debian:buster

# pkg-config added to `apt-get install` list because it is required by package
# digest-0.0.1.7.
RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y \
    curl build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 \
    libncurses-dev libncurses5 libtinfo5 libnuma-dev xz-utils g++ gcc \
    libc6-dev libffi-dev libgmp-dev make zlib1g-dev git gnupg netbase pkg-config

# This is added in an attempt to avoid the failure:
#   <stderr>: commitAndReleaseBuffer: invalid argument (invalid character)
ENV LANG="C.UTF-8"

RUN cd /tmp && \
    curl -L https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/clang+llvm-9.0.1-aarch64-linux-gnu.tar.xz --output /tmp/llvm.tar.xz && \
    unxz /tmp/llvm.tar.xz && \
    tar xfv /tmp/llvm.tar --strip-components 1 -C /usr && \
    rm /tmp/llvm.tar

# Stack's *.tar archive contains a directory that contains the 'stack'
# executable, hence the use of tar's '--strip-components 1' option.
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v2.11.1/stack-2.11.1-linux-aarch64.tar.gz --output /tmp/stack.tar.gz && \
    tar xfv /tmp/stack.tar.gz -C /usr/local/bin --strip-components 1 && \
    rm /tmp/stack.tar.gz

# RUN curl -sSL https://github.com/commercialhaskell/stack/releases/download/v2.7.1/stack-2.7.1-linux-aarch64.bin > /usr/local/bin/stack && \
RUN chmod +x /usr/local/bin/stack

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

RUN stack script --resolver lts-20.26 --compile /src/release.hs -- --version
RUN cp /src/release /home/stack
