release.hs
==========

This tool automates some aspects of releasing a new version of Stack. It
currently handles some tasks that need to be performed on each platform:
building the release, running some pre-release checks, and uploading binaries to
a Github release.

See [Checklist](../../doc/MAINTAINER_GUIDE.md) of
additional manual release steps.

Prerequisites
-------------

These must be installed in the PATH to use the release tool:

- stack
- git (for Windows, [msysgit](https://msysgit.github.io) is recommended).
- cabal (cabal-install)

To create a signed binary, you need:

- GPG installed and in the PATH (included with
  [msysgit](https://msysgit.github.io) on Windows)
- Your private key in GPG keyring.

To upload a binary to a Github release, you also need:

- A [Github authorization token](https://github.com/settings/tokens) with
  `public_repo` scope.
- Set `GITHUB_AUTH_TOKEN` environment variable to the authorization token.
- A [Github release](https://github.com/commercialhaskell/stack/releases)
  (probably as a draft) with a tag for the stack package's version (e.g.
  `vX.Y.Z`).

To create and upload Debian/Ubuntu packages, you need:

- deb-s3 installed (`sudo gem install deb-s3`).
- `dev@fpcomplete.com` secret key in GPG keyring.
- Set `AWS_SECRET_ACCESS_KEY` and `AWS_ACCESS_KEY_ID` environment variables with
  credentials that allow uploading to download.fpcomplete.com S3 bucket.

To create and upload Red Hat/CentOS packages, you need:

- [rpm-s3 installed](https://github.com/crohr/rpm-s3).
- `dev@fpcomplete.com` secret key in GPG keyring.
- Set `AWS_SECRET_ACCESS_KEY` and `AWS_ACCESS_KEY_ID` environment variables with
  credentials that allow uploading to download.fpcomplete.com S3 bucket.

To create and upload Arch packages, you need:

- [AWS CLI installed](http://docs.aws.amazon.com/cli/latest/userguide/installing.html).

Invocation
----------

Usage: `etc/scripts/release.hs [OPTIONS] TARGET`

The tool must be run in the root of the working tree.

### Options

The release tool is shake-based, so all standard shake options apply. In
addition, the following options are accepted:

* `--gpg-key`: override GPG key used to sign the distribution packages. By
  default the `dev@fpcomplete.com` key is used.
* `--github-auth-token`: override the Github authorization token.
* `--github-release-tag`: overrides the Github Release tag that binaries are
* `--allow-dirty`: by default, the `check` rule aborts if the working tree is
  dirty, but this will allow it to continue.
  uploaded to.

### Targets

* `release`: check, build, and upload.
* `check`: run pre-release checks.
* `build`: build and sign the binary distribution.
* `upload`: upload the binary distribution to the Github release.
* `ubuntu-packages`: build Ubuntu .deb packages.
* `ubuntu-upload`: upload Ubuntu .deb packages to private package repository.
* `debian-packages`: build Debian .deb packages.
* `debian-upload`: upload Debian .deb packages to private package repository.
* `centos-packages`: build CentOS .rpm packages.
* `centos-upload`: upload CentOS .rpm packages to private package repository.
* `fedora-packages`: build Fedora .rpm packages.
* `fedora-upload`: upload Fedora .rpm packages to private package repository.
