#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
export STACK_BUILD_DIR="$PWD"
cd "$(dirname "$0")/../../etc/vagrant/$1"

# Double 'vagrant up' is a workaround for FreeBSD
vagrant up || true
vagrant up

vagrant provision
vagrant rsync
vagrant ssh -c "set -xe; $3 export GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN; export STACK_RELEASE_GPG_KEY=${STACK_RELEASE_GPG_KEY:-0x575159689BEFB442}; gpg --import /vagrant/gpg-secret-key.asc~ || true; rm -f /vagrant/gpg-secret-key.asc~; cd /vagrant-build; for x in CONTRIBUTING ChangeLog; do rm -f doc/\$x.md; ln -s ../\$x.md doc/\$x.md; done; stack /vagrant/etc/scripts/release.hs --no-test-haddocks $2"
vagrant halt || vagrant halt -f
