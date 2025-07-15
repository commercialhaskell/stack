#!/usr/bin/env stack
-- stack --snapshot ghc-9.10.2 script --extra-dep acme-missiles-0.3@rev:0 --extra-dep stm-2.5.3.1@rev:1
import Acme.Missiles

main :: IO ()
main = launchMissiles
