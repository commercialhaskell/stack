#!/usr/bin/env bash
set -xe
"$(dirname "$0")/with-vagrant.sh" alpine-3.4-x86_64 "--binary-variant=static --static $* release"
"$(dirname "$0")/with-vagrant.sh" freebsd-10.3-amd64 "$* release" "export LANG=en_US.UTF-8;"
"$(dirname "$0")/with-vagrant.sh" debian-7-amd64 "$* release"
"$(dirname "$0")/with-vagrant.sh" debian-7-i386 "$* release"
"$(dirname "$0")/with-vagrant.sh" centos-6-x86_64 "--binary-variant=gmp4 $* release"
"$(dirname "$0")/with-vagrant.sh" centos-6-i386 "--binary-variant=gmp4 $* release"
