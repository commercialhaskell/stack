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

To create a signed binary package, you need:

- GPG installed and in the PATH (included with
  [msysgit](https://msysgit.github.io) on Windows)
- `dev@fpcomplete.com` secret key in GPG keyring.

To create signed Windows executables, you also need:

- `signtool.exe`, which is installed with the
  [Windows SDK](http://microsoft.com/en-us/download/confirmation.aspx?id=8279).
- "FP Complete, Corporation" code signing key installed. See
  [instructions for creating one with StartSSL](https://forum.startcom.org/viewtopic.php?p=5480&sid=143a360f30427e979f6c5b05c2df82cc#p5480).

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

Building
--------

Ensure that `~/.local/bin` is in your PATH, then:

    (cd etc/scripts && stack install)

(note: do not use `stack exec stack-release-script`, because certain parts of
the build do not work properly while in a `stack exec` context, especially on
Windows)

Invocation
----------

Usage: `stack-release-script [OPTIONS] TARGET`

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
* `build-<distro>-<ver>`: build package for Linux distribution.
* `upload-<distro>-<ver>`: upload package for Linux distribution to private package repository.
* `clean`: delete the build artifacts.

`<distro>` can have one of these values: `ubuntu`, `debian`, `centos`, `fedora`.  
`<ver>` is the version of the distribution (e.g., `14.04` for Ubuntu).
