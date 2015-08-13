#!/bin/bash -xe

# BUILD
stack --install-ghc build
[[ "$(stack path --local-install-root)/bin/stack" -nt "$OUTPUT_PKG" ]] || exit 0

# PKG
mkdir -p /fpm/usr/bin
strip -p --strip-unneeded --remove-section=.comment -o /fpm/usr/bin/stack $(stack path --local-install-root)/bin/stack
#mkdir -p /fpm/usr/share/man/man1
#gzip -c < man/man1/stack.1 >/fpm/usr/share/man/man1/stack.1.gz
mkdir -p /fpm/etc/bash_completion.d
/fpm/usr/bin/stack --bash-completion-script /usr/bin/stack >/fpm/etc/bash_completion.d/stack
fpm \
    -s dir \
    -t deb \
    -n stack \
    -v "$PKG_VERSION" \
    --deb-recommends git \
    --deb-recommends gnupg \
    -d g++ \
    -d gcc \
    -d libc6-dev \
    -d libffi-dev \
    -d libgmp-dev \
    -d make \
    -d xz-utils \
    -d zlib1g-dev \
    -C /fpm \
    -p "$OUTPUT_PKG" \
    -m "$PKG_MAINTAINER" \
    --description "$PKG_DESCRIPTION" \
    --license "$PKG_LICENSE" \
    --url "$PKG_URL" \
    etc usr
