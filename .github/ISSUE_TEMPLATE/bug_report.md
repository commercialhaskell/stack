---
name: Bug Report
about: Report a bug in Stack
---

Please follow the steps below for reporting a bug:

Make sure that you are using the latest release (currently stack-1.9.1).
See the [upgrade instructions](http://docs.haskellstack.org/en/stable/install_and_upgrade/#upgrade) to upgrade.

Please use the following schema for your bug report:

### General summary/comments (optional)

### Steps to reproduce

For example:

1. Remove directory *blah*.
2. Run command `stack blah`.
3. Edit file blah.
4. Run command `stack blah`.

Include any `.yaml` configuration if relevant.

### Expected

What you expected to see and happen.

### Actual

What actually happened.

If you suspect that a stack command misbehaved, please include the output of that command in `--verbose` mode.
If the output is larger than a page please paste the output in a [Gist](https://gist.github.com/).

```
$ stack <your command here> <args> --verbose
<output>
```

### Stack version

```
$ stack --version
Version 1.5.0, Git revision 63267f94d7c819cbecc2d59aa259d17240838e43 (4845 commits) x86_64 hpack-0.17.1
```

### Method of installation

* Official binary, downloaded from stackage.org or fpcomplete's package repository
* Via cabal-install
* An unofficial package repository (please specify which)
* Other (please specify)
