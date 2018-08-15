{-# LANGUAGE NoImplicitPrelude #-}
module Pantry.HackageSpec (spec) where

import Test.Hspec
import Pantry
import RIO

spec :: Spec
spec = it "update works" $ asIO $ void $ runPantryApp $ updateHackageIndex Nothing
