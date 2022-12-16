<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack docker` commands

~~~text
stack docker COMMAND

Available commands:
  pull                     Pull latest version of Docker image from registry
  reset                    Reset the Docker sandbox
~~~

Stack is able to build your code inside a Docker image, which means even more
reproducibility to your builds, since you and the rest of your team will always
have the same system libraries.

For further information, see the [Docker integration](docker_integration.md)
documentation.

## The `stack docker pull` command

~~~text
stack docker pull
~~~

`stack docker pull` pulls the latest version of the Docker image from the
registry.

## The `stack docker reset` command

~~~text
stack docker reset [--keep-home]
~~~

`stack docker reset` resets the Docker sandbox.

Pass the flag `--keep-home` to preserve the sandbox's home directory.
