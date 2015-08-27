# Custom stack snapshots

*WARNING*: Do not modify the existing snapshots!  It will make
dragons fly out of your nose!  Create a new one!

Exception:
* you're just adding a new package
* ?

## Overview

Stack supports custom snapshots since version 1.3.  The idea is
that you're able to create a file specifying all the packages you
want, include those extra dependencies, and make stack use it as
if it came from Stackage.

## Format

```
compiler: ghc-7.10.2

flags:
  aeson:
    old-locale: false

packages:
  - aeson-diff-0.1.1.2
```


## Creating a new snapshot

Note: start by just adding custom dependencies to extra-deps.
Once this change is approved and merged, make a separate task for creating a new snapshot.

  1. Update a `stack.yaml` to make it use the latest Stackage lts release via the `resolver` flag.

  2. Add as extra dependencies everything else needed.

  3. Build and test everything via `stack test`.

  4. create a new stack snapshot .yaml file with a different name from the last resolver

  5. copy the lts constraints from stackage into the yaml file. You can take the cabal.config from stackage for example, but you will need to remove the packages whose version is 'installed').

  6. generate the extra-dependencies packages constraints, and put those at the top of the package list

         stack exec -- ghc-pkg list $(stack exec -- ghc-pkg list | egrep 'stack-work' | sed 's/:$//' | sed 's/^/-f /' | xargs) | sed 's/^   /  -/'

  7. Make sure there are no packages that come from the "packages" section of your stack.yaml

  8. remove any packages from the lts constraints that are also in the extra-dependencies constraints

  9. Update stack.yaml to use the new resolver and make sure it all compiles

  10. Wipe out old custom snapshots to free disk space
