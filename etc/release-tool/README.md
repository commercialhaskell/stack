Stack release tool
==================

This tool automates some aspects of releasing a new version of Stack. It
currently handles some tasks that need to be performed on each platform:
building the release, running some pre-release checks, and uploading to Github.

See [Checklist](https://github.com/commercialhaskell/stack/wiki/Checklist) of
additional manual release steps.

Prerequisites
-------------

These must be installed in the PATH to use the release tool:

- stack
- Git
- GPG

(for Windows, [msysgit](https://msysgit.github.io) includes both Git and GPG).

To create a signed binary, you need:

- Private key installed in GPG.

To upload a binary to a Github release, you also need:

- A [Github authorization token](https://github.com/settings/tokens) with
  `public_repo` scope.
- Set `GITHUB_AUTH_TOKEN` environment variable to the authorization token.
- A [Github release](https://github.com/commercialhaskell/stack/releases)
  (probably as a draft) with a tag for the stack package's version (e.g.
  `vX.Y.Z`).

Build
-----

Build and install the release tool using:
```
(cd etc/release-tool && stack install)
```

Invocation
----------

Usage: `stack-release-tool [OPTIONS] TARGET`

The tool must be run in the root of the working tree.

### Options

stack-release-tool is shake-based, so all standard shake options apply. In
addition, the following options are accepted:

* `--gpg-key`: use a non-default GPG key to sign the binaries.
  dirty, but this will allow it to continue.
  uploaded to.
* `--github-auth-token`: override the Github authorization token.
* `--github-release-tag`: overrides the Github Release tag that binaries are
* `--allow-dirty`: by default, the `check` rule aborts if the working tree is

### Targets

* `release`: check, build, and upload.
* `check`: run pre-release checks.
* `build`: build and sign the binary distribution.
* `upload`: upload the binary distribution to the Github release.
