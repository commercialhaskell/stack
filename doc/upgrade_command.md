<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack upgrade` command

Either:

~~~text
stack upgrade [--binary-only] [--binary-platform ARG] [--force-download]
              [--[no-]only-local-bin] [--binary-version ARG] [--github-org ARG]
              [--github-repo ARG]
~~~

or:

~~~text
stack upgrade [--source-only] [--git] [--git-repo ARG] [--git-branch ARG]
~~~

`stack upgrade` will get a new version of Stack.

By default:

* the new version will be from an existing binary distribution. Pass the
  `--source-only` flag to specify compiling from source code. The
  `--binary-only` and `--source-only` flags are alternatives;

* the new version will not overwrite the existing version unless it is newer.
  Pass the `--force-download` flag to force a download;

* when an existing binary distribution is applicable, it will be put in Stack's
  local binary directory (see `stack path --local-bin`) and named `stack`
  (replacing any existing executable named `stack` there);

* if the current running Stack executable is named `stack` (or, on Windows,
  `stack.exe`) (this is case insensitive), an existing binary distribution will
  replace it. If the executable is located outside of Stack's local binary
  directory, pass the `--only-local-bin` flag to skip that step;

* if the current running Stack executable is named other than `stack` (and, on
  Windows, `stack.exe`), an existing binary distribution will only be put in
  Stack's local binary directory and named `stack`. Pass the
  `--no-only-local-bin` flag to replace also the current running executable;

* the new version will be the latest available. Pass the
  `--binary-version <version>` option to specify the version (this implies
  `--force-download`);

* the binary distribution will be sought from the GitHub organisation/user
  `commercialhaskell`. Pass the `--github-org <user>` option to specify a
  different GitHub user;

* the binary distribution will be sought from the GitHub repository `stack`.
  Pass the `--github-repo <repository>` option to specify a different
  repository; and

* the binary distribution will be sought for the current platform. Pass the
  `--binary-platform <platform>` option to specify a different platform
  (`<operating_system>-<architecture>-<suffix>`).

When compiling from source code, by default:

*   Stack will obtain the source code for the most recent version in the package
    index (eg Hackage). Pass the flag `--git` to specify the most recent version
    from the `master` branch of Stack's repository (pass the option
    `--git-branch <branch>` to specify a different branch and the option
    `--git-repo <repo_url>` to specify a different repository).

## Examples

* `stack upgrade` seeks an upgrade to the latest version of Stack available as a
  binary distribution for the platform, if newer.

* `stack upgrade --force-download` seeks an upgrade to the latest version of
  Stack available as a binary distribution for the platform, even if not newer.

* If the Stack executable is named `my-stack`, `my-stack upgrade` seeks only to
  put the latest version of Stack available as a binary distribution for the
  platform, if newer, in Stack's local binary directory and name it `stack`.
  `my-stack upgrade --no-only-local-bin` seeks also to upgrade `my-stack` to the
  latest version of Stack available.

* `stack upgrade --binary-version 2.13.1` seeks an upgrade to Stack 2.13.1 if
  available as a binary distribution for the platform, even if not newer.

* `stack upgrade --source-only` seeks an upgrade by building Stack with
  Stack from the latest version of the source code in the package index
  (i.e. Hackage).

* `stack upgrade --source-only --git` seeks an upgrade by building Stack with
  Stack from the latest version of the source code in the `master` branch of
  Stack's repository.
