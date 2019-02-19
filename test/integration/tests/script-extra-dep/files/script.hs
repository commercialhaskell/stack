#!/usr/bin/env stack
-- stack --resolver ghc-8.2.2 script --extra-dep acme-missiles-0.3@rev:0
import Acme.Missiles

main :: IO ()
main = launchMissiles
