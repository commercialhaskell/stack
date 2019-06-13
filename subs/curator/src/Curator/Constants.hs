module Curator.Constants
    ( snapshotFilename
    , constraintsFilename
    , snapshotsRepo
    , constraintsRepo
    ) where

snapshotFilename :: FilePath
snapshotFilename = "snapshot.yaml" 

constraintsFilename :: FilePath
constraintsFilename = "constraints.yaml"

snapshotsRepo :: String
snapshotsRepo = "commercialhaskell/stackage-next"

constraintsRepo :: String
constraintsRepo = "commercialhaskell/stackage-constraints-next"
