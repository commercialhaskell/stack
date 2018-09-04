<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>
# Terminology

This is a work-in-progress document covering terminology used by
Stack. It was started following the Pantry rewrite work in Stack
(likely to land as Stack 2.0), and contains some significant
changes/simplifications from previous terms.

__NOTE__ This document should *not be considered accurate* until this
note is removed.

Correct, new terminology

* Project package: anything listed in `packages` in stack.yaml
* Dependency: anything listed in extra-deps or a snapshot
* Target: package and/or component listed on the command line to be built. Can be either project package or dependency. If none specified, automatically targets all project packages

Outdated terminology to be purged:

* Wanted
* Local
* Snapshot package
