<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Customisation scripts

## GHC installation customisation

[:octicons-tag-24: 2.9.1](https://github.com/commercialhaskell/stack/releases/tag/v2.9.1)

On Unix-like operating systems and Windows, Stack's installation procedure can
be fully customised by placing a `sh` shell script (a 'hook') in the
[Stack root](stack_root.md) directory at `hooks/ghc-install.sh`. On Unix-like
operating systems, the script file must be made executable. The script is run by
the `sh` application (which is provided by MSYS2 on Windows).

The script **must** return an exit code of `0` and the standard output **must**
be the absolute path to the GHC binary that was installed. Otherwise Stack will
ignore the script and possibly fall back to its own installation procedure.

When `system-ghc: true`, the script is not run. That is because the two
mechanisms reflect distinct concepts, namely:

* `system-ghc: true` causes Stack to search the PATH for a version of GHC; and

* `hooks/ghc-install.sh` causes Stack to execute a script that is intended to
  send to standard output a path to a version of GHC. The path in question may
  or may not be in the PATH. The script may also do other things, including
  installation.

When `install-ghc: false`, the script is still run. That allows you to ensure
that only your script will install GHC and Stack won't default to its own
installation logic, even when the script fails.

The following environment variables are always available to the script:

* `HOOK_GHC_TYPE = "bindist" | "git" | "ghcjs"`

For "bindist", additional variables are:

* `HOOK_GHC_VERSION = <ver>`

For "git", additional variables are:

* `HOOK_GHC_COMMIT = <commit>`
* `HOOK_GHC_FLAVOR = <flavor>`

For "ghcjs", additional variables are:

* `HOOK_GHC_VERSION = <ver>`
* `HOOK_GHCJS_VERSION = <ver>`

An example script is:

~~~sh
#!/bin/sh

set -eu

case $HOOK_GHC_TYPE in
	bindist)
		# install GHC here, not printing to stdout, e.g.:
		#   command install $HOOK_GHC_VERSION >/dev/null
		;;
	git)
		>&2 echo "Hook doesn't support installing from source"
		exit 1
		;;
	*)
		>&2 echo "Unsupported GHC installation type: $HOOK_GHC_TYPE"
		exit 2
		;;
esac

echo "location/to/ghc/executable"
~~~

If the following script is installed by GHCup, GHCup makes use of it, so that if
Stack needs a version of GHC, GHCup takes over obtaining and installing that
version:

~~~sh
#!/bin/sh

set -eu

case $HOOK_GHC_TYPE in
    bindist)
        ghcdir=$(ghcup whereis --directory ghc "$HOOK_GHC_VERSION" || ghcup run --ghc "$HOOK_GHC_VERSION" --install) || exit 3
        printf "%s/ghc" "${ghcdir}"
        ;;
    git)
        # TODO: should be somewhat possible
        >&2 echo "Hook doesn't support installing from source"
        exit 1
        ;;
    *)
        >&2 echo "Unsupported GHC installation type: $HOOK_GHC_TYPE"
        exit 2
        ;;
esac
~~~
