module MyPackageA
    ( funcA
    ) where

import MyPackageB ( funcB )

funcA :: IO ()
funcA = funcB
