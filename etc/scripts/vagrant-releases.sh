#!/usr/bin/env bash
set -xe

#TODO: set up gpg-agent forwarding for package signing (see http://superuser.com/questions/161973/how-can-i-forward-a-gpg-key-via-ssh-agent).
gpg --export-secret-keys --armor "${STACK_RELEASE_GPG_KEY:-0x575159689BEFB442}" >"$(dirname "$0")/../../gpg-secret-key.asc~"

"$(dirname "$0")/with-vagrant.sh" freebsd-11.0-amd64 "$* upload" "export LANG=en_US.UTF-8;"
"$(dirname "$0")/with-vagrant.sh" debian-7-i386 "$* upload"

# Disabled because this is built by CI
#"$(dirname "$0")/with-vagrant.sh" debian-7-amd64 "$* release"

# Disabled because the static bindist will work
#"$(dirname "$0")/with-vagrant.sh" centos-6-x86_64 "--binary-variant=gmp4 $* upload"

# Disabled because GHC is not releasing 32-bit CentOS bindists
#"$(dirname "$0")/with-vagrant.sh" centos-6-i386 "--binary-variant=gmp4 $* upload"

# Disabled because GHC 8.2 doesn't work on Alpine
#"$(dirname "$0")/with-vagrant.sh" alpine-3.6-x86_64 "--binary-variant=static --static $* upload"
