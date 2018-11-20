release.hs
==========

This tool automates some aspects of releasing a new version of Stack. It
currently handles some tasks that need to be performed on each platform:
building the release, running some pre-release checks, and uploading binaries to
a Github release.

See [Checklist](../../doc/maintainers/releases.md) of
additional manual release steps.

Prerequisites
-------------

These must be installed in the PATH to use the release tool:

- stack
- git (for Windows, [msysgit](https://msysgit.github.io) is recommended).

To create a signed binary package, you need:

- GPG installed and in the PATH (included with
  [msysgit](https://msysgit.github.io) on Windows)
- `dev@fpcomplete.com` secret key in GPG keyring. You may also use the
  environment variable `STACK_RELEASE_GPG_KEY`, which should be
  set to the hexadecimal (0xLONG) identifier of the GPG key.

To upload a binary to a Github release, you also need:

- A [Github authorization token](https://github.com/settings/tokens) with
  `public_repo` scope.
- Set `GITHUB_AUTH_TOKEN` environment variable to the authorization token.
- A [Github release](https://github.com/commercialhaskell/stack/releases)
  (probably as a draft) with a tag for the stack package's version (e.g.
  `vX.Y.Z`).

Invocation
----------

Usage: `stack etc/scripts/release.hs [OPTIONS] TARGET`

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

You may also use the following environment variables in order to use a custom
GPG key:
* `STACK_RELEASE_GPG_KEY` should be set to the hexadecimal identifier (0xLONG) of the
  GPG key

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
