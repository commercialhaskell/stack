release.hs
==========

This tool automates some aspects of releasing a new version of Stack. It
currently handles some tasks that need to be performed on each platform:
building the release, running integration tests, and other pre-release checks.

See [Checklist](../../doc/maintainers/releases.md) of
additional manual release steps.

Prerequisites
-------------

These must be installed in the PATH to use the release tool:

- stack
- git (for Windows, [Git for Windows](https://gitforwindows.org/) is
  recommended).

Invocation
----------

Usage: `stack etc/scripts/release.hs [OPTIONS] TARGET`

The tool must be run in the root of the working tree.

### Options

The release tool is shake-based, so all standard shake options apply. In
addition, the following options are accepted:

* `--allow-dirty`: by default, the `check` rule aborts if the working tree is
  dirty, but this will allow it to continue.
* `--arch=ARCHITECTURE`: Architecture to build (e.g. 'i386' or 'x86_64').
* `--binary-variant=SUFFIX`: Extra suffix to add to binary executable archive
  filename.
* `--no-test-haddocks`: Disable testing building Haddock documentation.
* `--alpine`: Build a statically linked binary using an Alpine Docker image.
* `--build-args="ARG1 ARG2 ..."`: Additional arguments to pass to `stack build`.
* `--certificate-name=NAME`: Certificate name for code signing on Windows.

### Targets

* `release`: check and build.
* `check`: run pre-release checks.
* `build`: build and sign the binary distribution.
* `clean`: delete the build artifacts.
