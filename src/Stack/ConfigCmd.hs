{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.ConfigCmd
Description : Make changes to project or global configuration.
License     : BSD-3-Clause

Make changes to project or global configuration.
-}

module Stack.ConfigCmd
  ( cfgCmdSet
  , cfgCmdSetName
  , cfgCmdEnv
  , cfgCmdEnvName
  , cfgCmdBuildFiles
  , cfgCmdBuildFilesName
  , cfgCmdName
  ) where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Attoparsec.Text as P
                   ( Parser, parseOnly, skip, string, takeText, takeWhile
                   , takeWhile1
                   )
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Pantry ( loadSnapshot )
import           Path ( (</>), parent )
import qualified RIO.Map as Map
import           RIO.NonEmpty ( nonEmpty )
import qualified RIO.NonEmpty as NE
import           RIO.Process ( envVarsL )
import           Stack.Config
                   ( makeConcreteSnapshot, getProjectConfig
                   , getImplicitGlobalProjectDir
                   )
import           Stack.Constants ( stackDotYaml )
import           Stack.Prelude
import           Stack.Types.BuildConfig ( BuildConfig )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.ConfigMonoid
                   ( configMonoidInstallGHCName
                   , configMonoidInstallMsysName
                   , configMonoidRecommendStackUpgradeName
                   , configMonoidSystemGHCName
                   )
import           Stack.Types.ConfigSetOpts
                   ( CommandScope (..), ConfigCmdSet (..) ,configCmdSetScope )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GHCVariant ( HasGHCVariant )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.Runner ( globalOptsL )
import           Stack.Types.Snapshot ( AbstractSnapshot )
import           System.Environment ( getEnvironment )

-- | Type repesenting exceptions thrown by functions exported by the
-- "Stack.ConfigCmd" module.
data ConfigCmdException
  = NoProjectConfigAvailable
  deriving Show

instance Exception ConfigCmdException where
  displayException NoProjectConfigAvailable =
    "Error: [S-3136]\n"
    ++ "'config' command used when no project configuration available."

-- | Function underlying Stack's @config set@ command.
cfgCmdSet ::
     (HasConfig env, HasGHCVariant env)
  => ConfigCmdSet -> RIO env ()
cfgCmdSet cmd = do
  conf <- view configL
  configFilePath <-
    case configCmdSetScope cmd of
      CommandScopeProject -> do
        mstackYamlOption <- view $ globalOptsL . to (.stackYaml)
        mstackYaml <- getProjectConfig mstackYamlOption
        case mstackYaml of
          PCProject stackYaml -> pure stackYaml
          PCGlobalProject -> getImplicitGlobalProjectDir <&> (</> stackDotYaml)
          PCNoProject _extraDeps -> throwIO NoProjectConfigAvailable
          -- maybe modify the ~/.stack/config.yaml file instead?
      CommandScopeGlobal -> pure conf.userGlobalConfigFile
  rawConfig <- liftIO (readFileUtf8 (toFilePath configFilePath))
  config <- either throwM pure (Yaml.decodeEither' $ encodeUtf8 rawConfig)
  newValue <- cfgCmdSetValue (parent configFilePath) cmd
  let yamlLines = T.lines rawConfig
      cmdKeys = cfgCmdSetKeys cmd  -- Text
      newValue' = T.stripEnd $
        decodeUtf8With lenientDecode $ Yaml.encode newValue  -- Text
      file = toFilePath configFilePath  -- String
      hits = catMaybes $ NE.toList $ NE.map (inConfig config) cmdKeys
      primaryCmdKey = NE.last $ NE.head cmdKeys
  newYamlLines <- case hits of
    [] -> do
      prettyInfoL
        [ pretty configFilePath
        , flow "has been extended."
        ]
      pure $ writeLines yamlLines "" (NE.head cmdKeys) newValue'
    [(cmdKey, oldValue)] -> if oldValue == newValue && cmdKey == primaryCmdKey
      then do
        prettyInfoL
          [ pretty configFilePath
          , flow "already contained the intended configuration and remains \
                 \unchanged."
          ]
        pure yamlLines
      else do
        when (cmdKey /= primaryCmdKey) $
          prettyWarn $
               fillSep
                 [ pretty configFilePath
                 , flow "contained a synonym for"
                 , style Target (fromString $ T.unpack primaryCmdKey)
                 , parens (style Current (fromString $ T.unpack cmdKey))
                 , flow "which has been replaced."
                 ]
            <> line
        switchLine configFilePath cmdKey primaryCmdKey newValue' [] yamlLines
    _ -> do
      -- In practice, this warning should not be encountered because with
      -- snapshot and resolver (deprecated) present, Stack will not parse the
      -- YAML file.
      prettyWarnL
        [ pretty configFilePath
        , flow "contains more than one possible existing configuration and, \
               \consequently, remains unchanged."
        ]
      pure yamlLines
  liftIO $ writeFileUtf8 file (T.unlines newYamlLines)
 where
  -- This assumes that if the key does not exist, the lines that can be
  -- appended to include it are of a form like:
  --
  -- key1:
  --   key2:
  --     key3: value
  --
  writeLines yamlLines spaces cmdKeys value =
    case nonEmpty $ NE.tail cmdKeys of
      Nothing -> yamlLines <> [spaces <> NE.head cmdKeys <> ": " <> value]
      Just ks -> writeLines
                   (yamlLines <> [spaces <> NE.head cmdKeys <> ":"])
                   (spaces <> "  ")
                   ks
                   value

  inConfig v cmdKeys = case v of
    Yaml.Object obj ->
      let cmdKey = NE.head cmdKeys
      in  case KeyMap.lookup (Key.fromText cmdKey) obj of
            Nothing -> Nothing
            Just v' -> case nonEmpty $ NE.tail cmdKeys of
              Nothing -> Just (cmdKey, v')
              Just ks -> inConfig v' ks
    _ -> Nothing

  switchLine file cmdKey _ _ searched [] = do
    prettyWarnL
      [ style Current (fromString $ T.unpack cmdKey)
      , flow "was not found in YAML file"
      , pretty file
      , flow "in the form"
      , style Shell "key: value"
      , flow "on a single line. Multi-line formats for existing keys are not \
             \supported by the"
      , style Shell "config set"
      , flow "commands. The file's contents have not been changed."
      ]
    pure $ reverse searched
  switchLine file cmdKey cmdKey' newValue searched (oldLine:rest) =
    case parseOnly (parseLine cmdKey) oldLine of
      Left _ -> switchLine file cmdKey cmdKey' newValue (oldLine:searched) rest
      Right (kt, spaces1, spaces2, spaces3, comment) -> do
        let newLine = spaces1 <> renderKey cmdKey' kt <> spaces2 <>
                ":" <> spaces3 <> newValue <> comment
        prettyInfoL
          [ pretty file
          , flow "has been updated."
          ]
        pure $ reverse searched <> (newLine:rest)

  parseLine :: Text -> Parser (KeyType, Text, Text, Text, Text)
  parseLine key = do
    spaces1 <- P.takeWhile (== ' ')
    kt <- parseKey key
    spaces2 <- P.takeWhile (== ' ')
    skip (== ':')
    spaces3 <- P.takeWhile1 (== ' ')
    -- This assumes that the existing value contains no space characters, which
    -- is tolerable for current purposes.
    void $ takeWhile1 (/= ' ')
    -- This assumes that anything that follows the existing value is a comment,
    -- which is tolerable for current purposes.
    comment <- takeText
    pure (kt, spaces1, spaces2, spaces3, comment)

  -- If the key is, for example, install-ghc, this recognises install-ghc,
  -- 'install-ghc' or "install-ghc".
  parseKey :: Text -> Parser KeyType
  parseKey k =   parsePlainKey k
    <|> parseSingleQuotedKey k
    <|> parseDoubleQuotedKey k

  parsePlainKey :: Text -> Parser KeyType
  parsePlainKey key = do
    _ <- P.string key
    pure PlainKey

  parseSingleQuotedKey :: Text -> Parser KeyType
  parseSingleQuotedKey = parseQuotedKey SingleQuotedKey '\''

  parseDoubleQuotedKey :: Text -> Parser KeyType
  parseDoubleQuotedKey = parseQuotedKey DoubleQuotedKey '"'

  parseQuotedKey :: KeyType -> Char -> Text -> Parser KeyType
  parseQuotedKey kt c key = do
    skip (==c)
    _ <- P.string key
    skip (==c)
    pure kt

  renderKey :: Text -> KeyType -> Text
  renderKey key kt = case kt of
    PlainKey -> key
    SingleQuotedKey -> '\'' `T.cons` key `T.snoc` '\''
    DoubleQuotedKey -> '"' `T.cons` key `T.snoc` '"'

-- |Type representing types of representations of keys in YAML files.
data KeyType
  = PlainKey  -- ^ For example: install-ghc
  | SingleQuotedKey  -- ^ For example: 'install-ghc'
  | DoubleQuotedKey  -- ^ For example: "install-ghc"
  deriving (Eq, Show)

cfgCmdSetValue ::
     (HasConfig env, HasGHCVariant env)
  => Path Abs Dir -- ^ root directory of project
  -> ConfigCmdSet -> RIO env Yaml.Value
cfgCmdSetValue root (ConfigCmdSetSnapshot newSnapshot) =
  snapshotValue root newSnapshot
cfgCmdSetValue root (ConfigCmdSetResolver newSnapshot) =
  snapshotValue root newSnapshot
cfgCmdSetValue _ (ConfigCmdSetSystemGhc _ bool') = pure $ Yaml.Bool bool'
cfgCmdSetValue _ (ConfigCmdSetInstallGhc _ bool') = pure $ Yaml.Bool bool'
cfgCmdSetValue _ (ConfigCmdSetInstallMsys _ bool') = pure $ Yaml.Bool bool'
cfgCmdSetValue _ (ConfigCmdSetRecommendStackUpgrade _ bool') =
  pure $ Yaml.Bool bool'
cfgCmdSetValue _ (ConfigCmdSetDownloadPrefix _ url) = pure $ Yaml.String url

snapshotValue ::
     HasConfig env
  => Path Abs Dir -- ^ root directory of project
  -> Unresolved AbstractSnapshot
  -> RIO env Yaml.Value
snapshotValue root snapshot = do
  snapshot' <- resolvePaths (Just root) snapshot
  concreteSnapshot <- makeConcreteSnapshot snapshot'
  -- Check that the snapshot actually exists
  void $ loadSnapshot =<< completeSnapshotLocation concreteSnapshot
  pure (Yaml.toJSON concreteSnapshot)

cfgCmdSetKeys :: ConfigCmdSet -> NonEmpty (NonEmpty Text)
cfgCmdSetKeys (ConfigCmdSetSnapshot _) = [["snapshot"], ["resolver"]]
cfgCmdSetKeys (ConfigCmdSetResolver _) = [["resolver"], ["snapshot"]]
cfgCmdSetKeys (ConfigCmdSetSystemGhc _ _) = [[configMonoidSystemGHCName]]
cfgCmdSetKeys (ConfigCmdSetInstallGhc _ _) = [[configMonoidInstallGHCName]]
cfgCmdSetKeys (ConfigCmdSetInstallMsys _ _) = [[configMonoidInstallMsysName]]
cfgCmdSetKeys (ConfigCmdSetRecommendStackUpgrade _ _) =
  [[configMonoidRecommendStackUpgradeName]]
cfgCmdSetKeys (ConfigCmdSetDownloadPrefix _ _) =
  [["package-index", "download-prefix"]]

-- | The name of Stack's @config@ command.
cfgCmdName :: String
cfgCmdName = "config"

-- | The name of Stack's @config@ command's @set@ subcommand.
cfgCmdSetName :: String
cfgCmdSetName = "set"

-- | The name of Stack's @config@ command's @env@ subcommand.
cfgCmdEnvName :: String
cfgCmdEnvName = "env"

-- | The name of Stack's @config@ command's @build-files@ subcommand.
cfgCmdBuildFilesName :: String
cfgCmdBuildFilesName = "build-files"

data EnvVarAction = EVASet !Text | EVAUnset
  deriving Show

-- | Function underlying Stack's @config env@ command.
cfgCmdEnv :: EnvSettings -> RIO EnvConfig ()
cfgCmdEnv es = do
  origEnv <- liftIO $ Map.fromList . map (first fromString) <$> getEnvironment
  mkPC <- view $ configL . to (.processContextSettings)
  pc <- liftIO $ mkPC es
  let newEnv = pc ^. envVarsL
      actions = Map.merge
        (pure EVAUnset)
        (Map.traverseMissing $ \_k new -> pure (EVASet new))
        (Map.zipWithMaybeAMatched $ \_k old new -> pure $
            if fromString old == new
              then Nothing
              else Just (EVASet new))
        origEnv
        newEnv
      toLine key EVAUnset = "unset " <> encodeUtf8Builder key <> ";\n"
      toLine key (EVASet value) =
        encodeUtf8Builder key <> "='" <>
        encodeUtf8Builder (T.concatMap escape value) <> -- TODO more efficient to use encodeUtf8BuilderEscaped
        "'; export " <>
        encodeUtf8Builder key <> ";\n"
      escape '\'' = "'\"'\"'"
      escape c = T.singleton c
  putBuilder $ Map.foldMapWithKey toLine actions

-- | This function takes no settings and yields no action of interest. It is
-- 'Stack.Config.withBuildConfig' that yields the desired actions.
cfgCmdBuildFiles :: () -> RIO BuildConfig ()
cfgCmdBuildFiles () = pure ()
