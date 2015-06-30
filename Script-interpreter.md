You can use stack as a script interpreter.  Here is an example using the [turtle](http://www.stackage.org/package/turtle) package:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-2.9 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Turtle
main = echo "Hello World!"
```

Set the executable bit on the script, and then you can run it as a regular command (on Windows, you will have to run `stack <script>.hs`).

The first line is the usual "shebang" to use `stack` as a script interpreter.  The second line, which is required, provides additional options to stack (due to the common limitation of the "shebang" line only being allowed a single argument).  In this case, the options tell stack to use the lts-2.9 resolver, automatically install GHC if it is not already installed, and ensure the `turtle` package is available.
