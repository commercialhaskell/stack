#!/usr/bin/env bash
set -xe
"$(dirname "$0")/with-vagrant.sh" debian-7-amd64 "$* upload-ubuntu-12.04 upload-ubuntu-14.04 upload-ubuntu-14.10 upload-ubuntu-15.04 upload-ubuntu-15.10 upload-ubuntu-16.04 upload-debian-7 upload-debian-8"
"$(dirname "$0")/with-vagrant.sh" centos-7-x86_64 "$* upload-centos-7 upload-fedora-22 upload-fedora-23"
"$(dirname "$0")/with-vagrant.sh" centos-6-x86_64 "$* --binary-variant=gmp4 upload-centos-6"
