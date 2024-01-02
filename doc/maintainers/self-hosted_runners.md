<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Self-hosted runners

[GitHub Actions](https://docs.github.com/en/actions) is used to do CI on Stack.
The `linux-arm64` job of the `integration-tests.yml` workflow runs on a
self-hosted runner for Linux and ARM64.

The basic setup is:

* FP Complete has an Oracle Cloud account that provides a free tier that
  includes a really powerful ARM64 machine;
* within Oracle Cloud, FP Complete are running an Ubuntu/ARM64 instance; and
* on that instance, FP Complete are running the GitHub Runner software.

Occasionally Oracle will turn off the machine because:

* Oracle thinks it is not being used (because of the free tier); and/or
* other things, like disk space filling up.

## Managing the runner

With the appropriate authority installed on the server, the runner can be
managed remotely using SSH, with command `ssh ubuntu@arm-runner.stackage.org`.

The available disk space can be queried with command `df -h`; the relevant entry
is for filesystem `/dev/sda1`.

If the available space is low, that may be due to unncessary GHC versions
installed in Stack's `programs` directory.
