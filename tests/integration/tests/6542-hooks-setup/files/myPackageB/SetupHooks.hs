module SetupHooks where

import Distribution.Simple.SetupHooks.Internal
  ( ConfigureHooks (..), PreConfPackageHook, SetupHooks (..), noConfigureHooks
  , noPreConfPackageOutputs, noSetupHooks
  )
import LibD ( messageD )

setupHooks :: SetupHooks
setupHooks = noSetupHooks { configureHooks = myConfigureHooks }

myConfigureHooks :: ConfigureHooks
myConfigureHooks = noConfigureHooks { preConfPackageHook = Just myPreConfHook }

myPreConfHook :: PreConfPackageHook
myPreConfHook inputs = do
  putStrLn messageD
  pure $ noPreConfPackageOutputs inputs
