#!/usr/bin/env bash
set -xe
"$(dirname "$0")/with-vagrant.sh" debian-7-amd64 "--upload-label='Linux 64-bit, standard' $* release"
"$(dirname "$0")/with-vagrant.sh" debian-7-i386 "--upload-label='Linux 32-bit, standard' $* release"
"$(dirname "$0")/with-vagrant.sh" centos-6-x86_64 "--binary-variant=gmp4 --upload-label='Linux 64-bit, libgmp4 for CentOS 6.x' $* release"
"$(dirname "$0")/with-vagrant.sh" centos-6-i386 "--binary-variant=gmp4 --upload-label='Linux 32-bit, libgmp4 for CentOS 6.x' $* release"
