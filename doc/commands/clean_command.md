<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack clean` command

Either

~~~text
stack clean [PACKAGE] [--[no-]omit-this]
~~~

or

~~~text
stack clean --full
~~~

`stack clean` deletes build artefacts for one or more project packages.

By default:

* all project packages are cleaned. Pass one or more project package names to
  specify individual project packages; and

* the `dist` directory and all of its subdirectories in the Stack work directory
  for each relevant project package are deleted. Pass the flag `--omit-this` to
  omit, from cleaning, the `dist` work directory (see `stack path --dist-dir`)
  and its subdirectories currently in use.

`stack clean --full` deletes the Stack work directories of the project and its
project packages.
