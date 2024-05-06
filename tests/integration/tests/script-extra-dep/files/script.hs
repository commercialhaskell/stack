#!/usr/bin/env stack
-- stack --snapshot ghc-9.6.5 script --extra-dep acme-missiles-0.3@rev:0 --extra-dep stm-2.5.2.1@rev:0
import Acme.Missiles

main :: IO ()
main = launchMissiles
