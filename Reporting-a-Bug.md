When reporting a bug, please write in the following format (if you copy/paste this, that's even better):

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
> Here is the `stack ---version` output:
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

If the above output is larger than a page, paste it in a private [Gist](https://gist.github.com/) instead.

Include any `.yaml` configuration if relevant.

The more detailed your report, the faster it can be resolved and will ensure it is resolved in the right way. Once your bug has been resolved, the responsible will tag the issue as _Needs confirmation_ and assign the issue back to you. Once you have tested and confirmed that the issue is resolved, close the issue. If you are not a member of the project, you will be asked for confirmation and we will close it.