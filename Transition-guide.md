This is intended to help people familiar with using other Haskell build tools- like cabal-install, hsenv, cabal-dev, etc- with getting started with stack. The goal of stack is to be easy to use, so hopefully there won't be any gotchas. In our experience so far, most of the confusion comes from things you don't need to do in stack. For example:

* There's no need for explicit sandboxing, all builds in stack are isolated automatically
* The `install` command doesn't exist. Dependencies are built for you automatically, and your packages are always registered into a local package database

This page is just a stub right now, please help us expand! Also, the [[FAQ]] may be helpful.