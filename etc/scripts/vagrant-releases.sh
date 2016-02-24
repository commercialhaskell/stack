#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
init_wd="$(pwd)"

with_vagrant() {
  #TODO: set up gpg-agent forwarding for package signing (see http://superuser.com/questions/161973/how-can-i-forward-a-gpg-key-via-ssh-agent).
  gpg --export-secret-keys --armor dev@fpcomplete.com >.stack-work/gpg-secret-key.asc
  pushd "$init_wd/etc/vagrant/$1"
  vagrant up
  vagrant provision
  vagrant rsync
  vagrant ssh -c "export GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN; export AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID; export AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY; export AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION; export AWS_SESSION_TOKEN=$AWS_SESSION_TOKEN; gpg --import /vagrant/.stack-work/gpg-secret-key.asc; cd /vagrant && (cd etc/scripts && stack --install-ghc build) && \$(cd etc/scripts && stack exec which stack-release-script) --no-test-haddocks $2"
  vagrant halt
  popd
}

if [[ "$(basename "$0")" == "vagrant-releases.sh" ]]; then
  with_vagrant debian-7-amd64 "--upload-label='Linux 64-bit, standard' release"
  with_vagrant debian-7-i386 "--upload-label='Linux 32-bit, standard' release"
  with_vagrant centos-6-x86_64 "--binary-variant=gmp4 --upload-label='Linux 64-bit, libgmp4 for CentOS 6.x' release"
  with_vagrant centos-6-i386 "--binary-variant=gmp4 --upload-label='Linux 32-bit, libgmp4 for CentOS 6.x' release"
elif [[ "$(basename "$0")" == "vagrant-distros.sh" ]]; then
  with_vagrant debian-7-amd64 "upload-ubuntu-12.04 upload-ubuntu-14.04 upload-ubuntu-14.10 upload-ubuntu-15.04 upload-ubuntu-15.10 upload-debian-7 upload-debian-8"
  with_vagrant centos-7-x86_64 "upload-centos-7 upload-fedora-21 upload-fedora-22 upload-fedora-23"
  with_vagrant centos-6-x86_64 "--binary-variant=gmp4 upload-centos-6"
else
  echo "Unknown script name: $(basename "$0")" >&2
  exit 1
fi
