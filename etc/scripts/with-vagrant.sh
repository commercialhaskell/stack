#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
export STACK_BUILD_DIR="$PWD"
cd "$(dirname "$0")/../.."
#TODO: set up gpg-agent forwarding for package signing (see http://superuser.com/questions/161973/how-can-i-forward-a-gpg-key-via-ssh-agent).

gpg_key_name="${STACK_RELEASE_GPG_KEY_NAME:-dev@fpcomplete.com}" # :- means get from env or use default
gpg --export-secret-keys --armor "$gpg_key_name" >"gpg-secret-key.asc~"
cd "etc/vagrant/$1"

# Double 'vagrant up' is a workaround for FreeBSD
vagrant up || true
vagrant up

vagrant provision
vagrant rsync
vagrant ssh -c "set -xe; $3 export GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN; gpg --import /vagrant/gpg-secret-key.asc~ || true; cd /vagrant-build; for x in CONTRIBUTING ChangeLog; do rm -f doc/\$x.md; ln -s ../\$x.md doc/\$x.md; done; stack /vagrant/etc/scripts/release.hs --no-test-haddocks $2"
vagrant halt || vagrant halt -f
