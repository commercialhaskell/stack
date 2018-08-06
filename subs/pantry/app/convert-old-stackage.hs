{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import RIO
import Pantry
import Conduit
import Pantry.OldStackage
import RIO.FilePath
import RIO.Time (Day, toGregorian)
import RIO.Directory
import qualified Data.Yaml as Yaml
import Data.Aeson.Extended
import qualified RIO.Text as T
import Data.Text.Read (decimal)
import Path (parseAbsDir)

data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Show, Eq)

renderSnapName :: SnapName -> Text
renderSnapName (LTS x y) = T.pack $ concat ["lts-", show x, ".", show y]
renderSnapName (Nightly d) = T.pack $ "nightly-" ++ show d

parseSnapName :: Text -> Maybe SnapName
parseSnapName t0 =
    lts <|> nightly
  where
    lts = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, t2) <- Just $ decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ decimal t3
        return $ LTS x y
    nightly = do
        t1 <- T.stripPrefix "nightly-" t0
        Nightly <$> readMaybe (T.unpack t1)

snapshots :: MonadResource m => ConduitT i (SnapName, FilePath) m ()
snapshots = do
  sourceDirectory "lts-haskell" .| concatMapC go
  sourceDirectory "stackage-nightly" .| concatMapC go
  where
    go fp = do
      (name, ".yaml") <- Just $ splitExtension $ takeFileName fp
      snap <- parseSnapName $ fromString name
      Just (snap, fp)

data App = App
  { appSimpleApp :: !SimpleApp
  , appPantryConfig :: !PantryConfig
  }

simpleAppL :: Lens' App SimpleApp
simpleAppL = lens appSimpleApp (\x y -> x { appSimpleApp = y })

instance HasLogFunc App where
  logFuncL = simpleAppL.logFuncL
instance HasPantryConfig App where
  pantryConfigL = lens appPantryConfig (\x y -> x { appPantryConfig = y })

run :: RIO App a -> IO a
run f = runSimpleApp $ do
  sa <- ask
  stack <- getAppUserDataDirectory "stack"
  root <- parseAbsDir $ stack </> "pantry"
  withPantryConfig
    root
    defaultHackageSecurityConfig
    HpackBundled
    8
    $ \pc ->
      runRIO
        App
          { appSimpleApp = sa
          , appPantryConfig = pc
          }
        f

main :: IO ()
main = run $ do
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
      sd1 <- completeSnapshot sdOrig
      logInfo "Completing suceeded"
      let bs = Yaml.encode sd1
      {- FIXME
      writeFileBinary "tmp" bs
      sd2 <- loadPantry
      WithJSONWarnings iosd2 warnings <- Yaml.decodeThrow bs
      sd2 <- liftIO iosd2
      unless (null warnings) $ error $ unlines $ map show warnings
      logInfo "Decoding new ByteString succeeded"
      when (sd1 /= sd2) $ error $ "mismatch on " ++ show snap
      -}
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
