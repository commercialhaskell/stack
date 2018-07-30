{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Stack.Prelude
import Stack.Types.Resolver
import Stack.Types.Runner
import Stack.Runners
import Stack.Options.GlobalParser
import Conduit
import Pantry.OldStackage
import RIO.FilePath
import RIO.Time (toGregorian)
import RIO.Directory
import qualified Data.Yaml as Yaml
import Data.Aeson.Extended

snapshots :: MonadResource m => ConduitT i (SnapName, FilePath) m ()
snapshots = do
  sourceDirectory "lts-haskell" .| concatMapC go
  sourceDirectory "stackage-nightly" .| concatMapC go
  where
    go fp = do
      (name, ".yaml") <- Just $ splitExtension $ takeFileName fp
      snap <- parseSnapName $ fromString name
      Just (snap, fp)

main :: IO ()
main = withConfigAndLock (globalOptsFromMonoid True ColorAuto mempty) $ do
  _ <- updateHackageIndex Nothing
  runConduitRes $ snapshots .| mapM_C (lift . go)
  where
  go (snap, fp) = do
    let destFile = "stackage-snapshots" </>
          (case snap of
            LTS x y -> "lts" </> show x </> show y <.> "yaml"
            Nightly date ->
              let (y, m, d) = toGregorian date
               in "nightly" </> show y </> show m </> show d <.> "yaml"
          )
    unlessM (doesFileExist destFile) $ do
      logInfo $ "Converting " <> display (renderSnapName snap) <> " from " <> fromString fp <> " into " <> fromString destFile
      sdOrig <- parseOldStackage
               (case snap of
                  LTS x y -> Left (x, y)
                  Nightly d -> Right d)
               (renderSnapName snap)
               fp
      logInfo "Decoding suceeded"
      sd1 <- completeSnapshot Nothing sdOrig
      logInfo "Completing suceeded"
      let bs = Yaml.encode sd1
      writeFileBinary "tmp" bs
      WithJSONWarnings sd2 warnings <- Yaml.decodeThrow bs
      unless (null warnings) $ error $ unlines $ map show warnings
      logInfo "Decoding new ByteString succeeded"
      when (sd1 /= sd2) $ error $ "mismatch on " ++ show snap
      createDirectoryIfMissing True (takeDirectory destFile)
      withSinkFileCautious destFile $ \sink -> runConduit $ yield bs .| sink

    {-
  sd <- loadResolver $ ResolverStackage $ LTS 12 0
  
  error $ show sd
  {-
  locs <- forM (sdLocations sd) completePackageLocation
  let sd' = sd { sdLocations = locs }
  error $ show sd'
  -}
    -}
