{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types and functions related to Stack's @query@ command.
module Stack.Query
  ( queryCmd
  , queryBuildInfo
  ) where

import           Data.Aeson ( Value (Object, Array), (.=), object )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.List ( isPrefixOf )
import qualified Data.Text as T
import           Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Text.IO as TIO
import           Data.Text.Read ( decimal )
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import           Path ( parent )
import           Stack.Build.Source ( projectLocalPackages )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Types.BuildConfig ( wantedCompilerVersionL )
import           Stack.Types.Compiler ( compilerVersionText )
import           Stack.Types.EnvConfig ( HasEnvConfig, actualCompilerVersionL )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.Package ( LocalPackage (..), Package (..) )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Query"module.
data QueryException
  = SelectorNotFound ![Text]
  | IndexOutOfRange ![Text]
  | NoNumericSelector ![Text]
  | CannotApplySelector !Value ![Text]
  deriving (Show, Typeable)

instance Exception QueryException where
  displayException (SelectorNotFound sels) =
    err "[S-4419]" "Selector not found" sels
  displayException (IndexOutOfRange sels) =
    err "[S-8422]" "Index out of range" sels
  displayException (NoNumericSelector sels) =
    err "[S-4360]" "Encountered array and needed numeric selector" sels
  displayException (CannotApplySelector value sels) =
    err "[S-1711]" ("Cannot apply selector to " ++ show value) sels

-- | Helper function for 'QueryException' instance of 'Show'
err :: String -> String -> [Text] -> String
err msg code sels = "Error: " ++ code ++ "\n" ++ msg ++ ": " ++ show sels

-- | Function underlying the @stack query@ command.
queryCmd ::
     [String]
     -- ^ Selectors.
  -> RIO Runner ()
queryCmd selectors = withConfig YesReexec $
  withDefaultEnvConfig $ queryBuildInfo $ map T.pack selectors

-- | Query information about the build and print the result to stdout in YAML
-- format.
queryBuildInfo ::
     HasEnvConfig env
  => [Text] -- ^ Selectors.
  -> RIO env ()
queryBuildInfo selectors0 =
      rawBuildInfo
  >>= select id selectors0
  >>= liftIO . TIO.putStrLn . addGlobalHintsComment . decodeUtf8 . Yaml.encode
 where
  select _ [] value = pure value
  select front (sel:sels) value =
    case value of
      Object o ->
        case KeyMap.lookup (Key.fromText sel) o of
          Nothing -> throwIO $ SelectorNotFound sels'
          Just value' -> cont value'
      Array v ->
        case decimal sel of
          Right (i, "")
            | i >= 0 && i < V.length v -> cont $ v V.! i
            | otherwise -> throwIO $ IndexOutOfRange sels'
          _ -> throwIO $ NoNumericSelector sels'
      _ -> throwIO $ CannotApplySelector value sels'
   where
    cont = select (front . (sel:)) sels
    sels' = front [sel]
  -- Include comments to indicate that this portion of the "stack
  -- query" API is not necessarily stable.
  addGlobalHintsComment
    | null selectors0 = T.replace globalHintsLine ("\n" <> globalHintsComment <> globalHintsLine)
    -- Append comment instead of pre-pending. The reasoning here is
    -- that something *could* expect that the result of 'stack query
    -- global-hints ghc-boot' is just a string literal. Seems easier
    -- for to expect the first line of the output to be the literal.
    | ["global-hints"] `isPrefixOf` selectors0 = (<> ("\n" <> globalHintsComment))
    | otherwise = id
  globalHintsLine = "\nglobal-hints:\n"
  globalHintsComment = T.concat
    [ "# Note: global-hints is experimental and may be renamed / removed in the future.\n"
    , "# See https://github.com/commercialhaskell/stack/issues/3796"
    ]

-- | Get the raw build information object
rawBuildInfo :: HasEnvConfig env => RIO env Value
rawBuildInfo = do
  locals <- projectLocalPackages
  wantedCompiler <- view $ wantedCompilerVersionL.to (utf8BuilderToText . display)
  actualCompiler <- view $ actualCompilerVersionL.to compilerVersionText
  pure $ object
    [ "locals" .= Object (KeyMap.fromList $ map localToPair locals)
    , "compiler" .= object
        [ "wanted" .= wantedCompiler
        , "actual" .= actualCompiler
        ]
    ]
 where
  localToPair lp =
    (Key.fromText $ T.pack $ packageNameString $ packageName p, value)
   where
    p = lpPackage lp
    value = object
      [ "version" .= CabalString (packageVersion p)
      , "path" .= toFilePath (parent $ lpCabalFile lp)
      ]
