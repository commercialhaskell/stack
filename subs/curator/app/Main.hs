{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import Curator
import Curator.StackageConstraints
import Data.Yaml (encodeFile)

main :: IO ()
main = runSimpleApp $ do
  loadSC "build-constraints.yaml" >>= liftIO . encodeFile "constraints.yaml"
