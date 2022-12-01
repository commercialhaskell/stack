<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack clean` command

Either

~~~text
stack clean [PACKAGE]
~~~

or

~~~text
stack clean --full
~~~

`stack clean` deletes build artefacts for one or more project packages specified
as arguments. If no project packages are specified, all project packages are
cleaned.

`stack clean --full` deletes the project's Stack working directory.
