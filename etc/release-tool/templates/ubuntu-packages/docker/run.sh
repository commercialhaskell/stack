#!/bin/bash -xe
fpmdir=/var/tmp/fpm/stack_$DEB_VERSION

# BUILD
stack build
[[ $(stack path --local-install-root)/bin/stack -nt $OUTPUT_DEB ]] || exit 0

# PKG
mkdir -p $fpmdir/usr/bin
strip -p --strip-unneeded --remove-section=.comment -o $fpmdir/usr/bin/stack $(stack path --local-install-root)/bin/stack
mkdir -p $fpmdir/usr/share/man/man1
gzip -c < man/man1/stack.1 >$fpmdir/usr/share/man/man1/stack.1.gz
mkdir -p $fpmdir/etc/bash_completion.d
$fpmdir/usr/bin/stack --bash-completion-script /usr/bin/stack >$fpmdir/etc/bash_completion.d/stack
fpm \
    -s dir \
    -t deb \
    -n stack \
    -v $DEB_VERSION \
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
    -C $fpmdir \
    -p $OUTPUT_DEB \
    -m "$PKG_MAINTAINER" \
    --description "$PKG_DESCRIPTION" \
    --license "$PKG_LICENSE" \
    --url "$PKG_URL" \
    etc usr
