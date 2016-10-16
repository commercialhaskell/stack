# Contributors Guide

## Bug Reports

Please [open an issue](https://github.com/commercialhaskell/stack/issues/new)
and use the provided template to include all necessary details.

The more detailed your report, the faster it can be resolved and will ensure it
is resolved in the right way. Once your bug has been resolved, the responsible
will tag the issue as _Needs confirmation_ and assign the issue back to you.
Once you have tested and confirmed that the issue is resolved, close the issue.
If you are not a member of the project, you will be asked for confirmation and
we will close it.


## Documentation

If you would like to help with documentation, please note that for most cases
the Wiki has been deprecated in favor of markdown files placed in a new `/doc`
subdirectory of the repository itself. Please submit a
[pull request](https://help.github.com/articles/using-pull-requests/) with your
changes/additions.

The documentation is rendered on [haskellstack.org](http://haskellstack.org) by
readthedocs.org using Sphinx and CommonMark. Since links and formatting vary
from GFM, please check the documentation there before submitting a PR to fix
those.  In particular, links to other documentation files intentionally have
`.html` extensions instead of `.md`, unfortunately (see
[#1506](https://github.com/commercialhaskell/stack/issues/1506) for details).

If your changes move or rename files, or subsume Wiki content, please continue
to leave a file/page in the old location temporarily, in addition to the new
location. This will allow users time to update any shared links to the old
location. Please also update any links in other files, or on the Wiki, to point
to the new file location.


## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve `stack`, pull requests are most welcome. It's a good idea to
[submit an issue](https://github.com/commercialhaskell/stack/issues/new) to
discuss the change before plowing into writing code.

If you'd like to help out but aren't sure what to work on, look for issues with
the
[awaiting pr](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pr%22)
label. Issues that are suitable for newcomers to the codebase have the
[newcomer](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pr%22+label%3Anewcomer)
label. Best to post a comment to the issue before you start work, in case anyone
has already started.

Please include a
[ChangeLog](https://github.com/commercialhaskell/stack/blob/master/ChangeLog.md)
entry and
[documentation](https://github.com/commercialhaskell/stack/tree/master/doc/)
updates with your pull request.

## Code Quality

The Stack projects uses [HLint](https://github.com/ndmitchell/hlint) as a code
quality tool.

Note that stack contributors need not dogmatically follow the suggested hints
but are encouraged to debate their usefulness. If you find a hint is not useful
and detracts from readability, consider marking it in the [configuration
file](https://github.com/commercialhaskell/stack/blob/master/HLint.hs) to
be ignored. Please refer to the [HLint manual](https://github.com/ndmitchell/hlint#ignoring-hints)
for configuration syntax.

Quoting [@mgsloan](https://github.com/commercialhaskell/stack/pulls?utf8=%E2%9C%93&q=is%3Apr%20author%3Amgsloan):

> We are optimizing for code clarity, not code concision or what HLint thinks.

You can install HLint with stack. You might want to install it in the global
project in case you run into dependency conflicts. HLint can report hints in
your favourite text editor. Refer to the HLint repository for more details.

To install:

```
stack install hlint
```

Once installed, you can check your changes with:

```
hlint src/ test/ --cpp-simple --hint=HLint.hs
```

Where `--cpp-simple` strips `#` lines and `--hint` explicitly specifies the
configuration file.
