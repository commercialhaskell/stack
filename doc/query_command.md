<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack query` command

:octicons-beaker-24: Experimental

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

~~~text
stack query [SELECTOR...]
~~~

`stack query` outputs certain build information. For example, for a
multi-package project `multi` specifying snapshot `lts-19.25` (GHC 9.0.2) and
with two local packages, `my-package-A` (version 0.1.0.0) and `my-package-B`
(version 0.2.0.0), command `stack query` outputs:

~~~text
compiler:
  actual: ghc-9.0.2
  wanted: ghc-9.0.2
locals:
  my-package-A:
    path: <absolute_path_to>\multi\my-package-A\
    version: 0.1.0.0
  my-package-B:
    path: <absolute_path_to>\multi\my-package-B\
    version: 0.2.0.0
~~~

The component parts of the information can be specified using 'selectors' with
the command. In the example above the selectors include `compiler`,
`compiler actual`, `locals`, `locals my-package-A`, and
`locals my-package-A version`. For example, commanding:

~~~text
stack query locals my-package-B path
~~~

results in output:

~~~text
<absolute_path_to>\multi\my-package-B\
~~~
