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
output stream the version of the package in the specified snapshot, unless the
package comes with GHC on Unix-like operating systems. If the package name
cannot be found in the snapshot, the command will fail, identifying only the
package(s) that did not appear in the snapshot.

More than one package name can be specified.

`stack --snapshot <snapshot> list` will send to the standard output stream a
list of all the packages in the specified snapshot, except those which come with
GHC on Unix-like operating systems.

For example:

~~~text
stack list base unix Win32 acme-missiles pantry
base-4.19.0.0
unix-2.8.5.0
Win32-2.13.4.0
acme-missiles-0.3
pantry-0.9.3.1

stack list paltry
Could not find package paltry, updating
...
Package index cache populated
Error: [S-4926]
       * Could not find package paltry on Hackage. Perhaps you meant one of:
         pantry, pretty, pasty, xattr, alloy, para, pappy, alure, polar and
         factory.

stack --snapshot lts-22.7 list base unix Win32 acme-missiles pantry
Error: [S-4926]
       * Package does not appear in snapshot: base.
       * Package does not appear in snapshot: unix.
       * Package does not appear in snapshot: Win32.
       * Package does not appear in snapshot: acme-missiles.

stack --snapshot lts-22.7 list pantry
pantry-0.9.3.1

stack --snapshot lts-22.7 list
AC-Angle-1.0
ALUT-2.4.0.3
...
zstd-0.1.3.0
zxcvbn-hs-0.3.6
~~~
