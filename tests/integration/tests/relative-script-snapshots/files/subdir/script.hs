#!/usr/bin/env stack
-- stack --resolver snapshot.yaml script
import Acme.Missiles

main :: IO ()
main = launchMissiles
