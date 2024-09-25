---
title: Get involved
---
<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Get involved

## Feedback and discussion

* For general comments, feedback and support, please post to the
  [Haskell Community](https://discourse.haskell.org/about) forum.
* For bugs, issues, or requests, please
  [open an issue](https://github.com/commercialhaskell/stack/issues/new).
* When using Stack Overflow, please use the
  [haskell-stack](http://stackoverflow.com/questions/tagged/haskell-stack) tag.

## How to contribute to the maintenance or development of Stack

A [guide](../CONTRIBUTING.md) is provided to help potential contributors to the
Stack project.

If you have already installed a version of Stack and the
[Git application](https://git-scm.com/) the followings steps should get you
started with building Stack from source with Stack:

1.  Clone the `stack` repository from GitHub with the command:

    ~~~text
    git clone https://github.com/commercialhaskell/stack.git
    ~~~

2.  Change the current working directory to the cloned `stack` directory with
    the command:

    ~~~text
    cd stack
    ~~~

3.  Build the `stack` executable using a preexisting installation of Stack with
    the command:

    ~~~text
    stack build
    ~~~

4.  Once the `stack` executable has been built, check its version with the
    command:

    ~~~text
    stack exec -- stack --version
    ~~~

    Make sure the version is the latest one.

5.  In the GitHub repository's issue tracker, look for issues tagged with
    [newcomer friendly](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3a%22newcomer+friendly%22)
    and
    [awaiting pull request](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3A%22awaiting+pull+request%22)
    labels.

If you need to check your changes quickly command:

~~~text
stack repl
~~~

and then, at the REPL's prompt, command:

~~~text
:main --stack-root=<path_to_root> --stack-yaml=<path_to_stack.yaml> <COMMAND>
~~~

This allows you to set a special Stack root (instead of the default Stack root)
and to target your commands at a particular `stack.yaml` file instead of the one
found in the current directory.
