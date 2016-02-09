# Contributors Guide

## Bug Reports

Before reporting a bug, please ensure that you are using the latest release (currently stack-1.0.2).  See the [upgrade instructions](http://docs.haskellstack.org/en/stable/install_and_upgrade.html#upgrade) to upgrade.

When reporting a bug, please write in the following format:

> [Any general summary/comments if desired]

> **Steps to reproduce:**

> 1. _Remove directory *blah*._
> 2. _Run command `stack blah`._
> 3. _Edit file blah._
> 3. _Run command `stack blah`._

> **Expected:**

> _What I expected to see and happen._

> **Actual:**

> _What actually happened._
>
> Here is the `stack --version` output:
>
> ```
> $ stack --version
> Version 0.0.2, Git revision 6a86ee32e5b869a877151f74064572225e1a0398
> ```
> Here is the command I ran **with `--verbose`**:
>
> ```
> $ stack <your command here> <args> --verbose
> <output>
> ```

With `--verbose` mode we can see what the tool is doing and when. Without this output it is much more difficult to surmise what's going on with your issue. If the above output is larger than a page, paste it in a private [Gist](https://gist.github.com/) instead.

Include any `.yaml` configuration if relevant.

The more detailed your report, the faster it can be resolved and will ensure it is resolved in the right way. Once your bug has been resolved, the responsible will tag the issue as _Needs confirmation_ and assign the issue back to you. Once you have tested and confirmed that the issue is resolved, close the issue. If you are not a member of the project, you will be asked for confirmation and we will close it.


## Documentation

If you would like to help with documentation, please note that for most cases the Wiki has been deprecated in favor of markdown files placed in a new `/doc` subdirectory of the repository itself. Please submit a [pull request](https://help.github.com/articles/using-pull-requests/) with your changes/additions.

The documentation is rendered on [haskellstack.org](http://haskellstack.org) by
readthedocs.org using Sphinx and CommonMark. Since links and formatting vary
from GFM, please check the documentation there before submitting a PR to fix
those.  In particular, links to other documentation files intentionally have
`.html` extensions instead of `.md`, unfortunately (see
[#1506](https://github.com/commercialhaskell/stack/issues/1506) for details).

If your changes move or rename files, or subsume Wiki content, please continue to leave a file/page in the old location temporarily, in addition to the new location. This will allow users time to update any shared links to the old location. Please also update any links in other files, or on the Wiki, to point to the new file location.


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
