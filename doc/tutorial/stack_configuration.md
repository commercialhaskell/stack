  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 12. Stack configuration

Whenever you run something with Stack, it needs a project-level configuration
file. The algorithm Stack uses to find such a file is:

1. Check for a `--stack-yaml` option on the command line
2. Check for a `STACK_YAML` environment variable
3. Check the current directory and all ancestor directories for a `stack.yaml`
   file

The first two provide a convenient method for using an alternate configuration.
For example: `stack build --stack-yaml stack-ghc-9.2.3.yaml` can be used by your
CI system to check your code against GHC 9.2.3. Setting the `STACK_YAML`
environment variable can be convenient if you're going to be running commands
like `stack ghc` in other directories, but you want to use the configuration you
defined in a specific project.

If Stack does not find a project level configuration file in any of the three
specified locations, the *implicit global* logic kicks in. You've probably
noticed that phrase a few times in the output from commands above. Implicit
global is essentially a hack to allow Stack to be useful in a non-project
setting. When no implicit global configuration file exists, Stack creates one
for you with the latest LTS snapshot. This allows you to do things like:

* compile individual files easily with `stack ghc`
* build executables without starting a project, e.g. `stack install pandoc`

Keep in mind that there's nothing magical about this implicit global
configuration. It has no effect on projects at all. Every package you install
with it is put into isolated databases just like everywhere else. The only magic
is that it's the catch-all project whenever you're running Stack somewhere else.
