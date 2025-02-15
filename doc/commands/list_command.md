<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack list` command

[:octicons-tag-24: 2.7.1](https://github.com/commercialhaskell/stack/releases/tag/v2.7.1)

~~~text
stack list [PACKAGE]
~~~

`stack list <package_name>` will send to the standard output stream the latest
version of the package from Hackage. If the package name cannot be found on
Hackage, even after updating the package index, suggestions (not  necessarily
good ones) will be made about the intended package name.

`stack --snapshot <snapshot> list <package_name>` will send to the standard
output stream the version of the package included in the specified snapshot
(either directly or indirectly, if a boot package of the compiler specified by
the snapshot). If the package name cannot be found in the snapshot, the command
will fail, identifying only the package(s) that did not appear in the snapshot.

More than one package name can be specified.

`stack --snapshot <snapshot> list` will send to the standard output stream a
list of all the packages included directly in the specified snapshot (that is,
excluding those included only indirectly as a boot package of the compiler
specified by the snapshot).

For example:

~~~text
stack list base unix Win32 acme-missiles pantry
base-4.21.0.0
unix-2.8.6.0
Win32-2.14.1.0
acme-missiles-0.3
pantry-0.10.0

stack list paltry
Could not find package paltry, updating
...
Package index cache populated
Error: [S-4926]
       * Could not find package paltry on Hackage. Perhaps you meant one of:
         tasty, retry, path, pretty, pasty, xattr, alloy, para, pappy and
         alure.

stack --snapshot lts-23.8 list base unix Win32 acme-missiles pantry
Error: [S-4926]
       * Package does not appear in snapshot (directly or indirectly): acme-missiles.

stack --snapshot lts-23.8 list base unix Win32 pantry
base-4.19.2.0
unix-2.8.6.0
Win32-2.13.4.0
pantry-0.10.0

stack --snapshot lts-23.8 list
AC-Angle-1.0
ALUT-2.4.0.3
...
zot-0.0.3
zstd-0.1.3.0
~~~
