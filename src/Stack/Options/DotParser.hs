{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.DotParser where

import           Data.Char ( isSpace )
import           Data.List.Split ( splitOn )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Types.PackageName ( mkPackageName )
import           Options.Applicative
                   ( CommandFields, Mod, Parser, auto, command, help, idm, info
                   , long, metavar, option, progDesc, showDefault, strOption
                   , subparser, switch, value
                   )
import           Options.Applicative.Builder.Extra ( boolFlags, textOption )
import           Stack.Dot
                   ( DotOpts (..), ListDepsFormat (..), ListDepsFormatOpts (..)
                   , ListDepsOpts (..)
                   )
import           Stack.Options.BuildParser ( flagsParser, targetsParser )
import           Stack.Prelude

-- | Parser for arguments to `stack dot`
dotOptsParser :: Bool -> Parser DotOpts
dotOptsParser externalDefault = DotOpts
  <$> includeExternal
  <*> includeBase
  <*> depthLimit
  <*> fmap (maybe Set.empty $ Set.fromList . splitNames) prunedPkgs
  <*> targetsParser
  <*> flagsParser
  <*> testTargets
  <*> benchTargets
  <*> globalHints
 where
  includeExternal = boolFlags externalDefault
    "external"
    "inclusion of external dependencies"
    idm
  includeBase = boolFlags True
    "include-base"
    "inclusion of dependencies on base"
    idm
  depthLimit = optional (option auto
    (  long "depth"
    <> metavar "DEPTH"
    <> help "Limit the depth of dependency resolution (Default: No limit)"
    ))
  prunedPkgs = optional (strOption
    (  long "prune"
    <> metavar "PACKAGES"
    <> help "Prune each package name from the comma separated list of package \
            \names PACKAGES"
    ))
  testTargets = switch
    (  long "test"
    <> help "Consider dependencies of test components"
    )
  benchTargets = switch
    (  long "bench"
    <> help "Consider dependencies of benchmark components"
    )

  splitNames :: String -> [PackageName]
  splitNames = map
    ( mkPackageName
    . takeWhile (not . isSpace)
    . dropWhile isSpace
    ) . splitOn ","

  globalHints = switch
    (  long "global-hints"
    <> help "Do not require an install GHC; instead, use a hints file for \
            \global packages"
    )

separatorParser :: Parser Text
separatorParser = fmap
  escapeSep
  ( textOption
      (  long "separator"
      <> metavar "SEP"
      <> help "Separator between package name and package version."
      <> value " "
      <> showDefault
      )
  )
 where
  escapeSep s = T.replace "\\t" "\t" (T.replace "\\n" "\n" s)

licenseParser :: Parser Bool
licenseParser = boolFlags False
  "license"
  "printing of dependency licenses instead of versions"
  idm

listDepsFormatOptsParser :: Parser ListDepsFormatOpts
listDepsFormatOptsParser = ListDepsFormatOpts
  <$> separatorParser
  <*> licenseParser

listDepsTreeParser :: Parser ListDepsFormat
listDepsTreeParser =  ListDepsTree <$> listDepsFormatOptsParser

listDepsTextParser :: Parser ListDepsFormat
listDepsTextParser = ListDepsText <$> listDepsFormatOptsParser

listDepsJsonParser :: Parser ListDepsFormat
listDepsJsonParser = pure ListDepsJSON

listDepsConstraintsParser :: Parser ListDepsFormat
listDepsConstraintsParser = pure ListDepsConstraints

toListDepsOptsParser :: Parser ListDepsFormat -> Parser ListDepsOpts
toListDepsOptsParser formatParser = ListDepsOpts
  <$> formatParser
  <*> dotOptsParser True

formatSubCommand ::
     String
  -> String
  -> Parser ListDepsFormat
  -> Mod CommandFields ListDepsOpts
formatSubCommand cmd desc formatParser =
  command cmd (info (toListDepsOptsParser formatParser) (progDesc desc))

-- | Parser for arguments to `stack ls dependencies`.
listDepsOptsParser :: Parser ListDepsOpts
listDepsOptsParser = subparser
      (  formatSubCommand
           "text"
           "Print dependencies as text (default)"
           listDepsTextParser
      <> formatSubCommand
           "cabal"
           "Print dependencies as exact Cabal constraints"
           listDepsConstraintsParser
      <> formatSubCommand
           "tree"
           "Print dependencies as tree"
           listDepsTreeParser
      <> formatSubCommand
           "json"
           "Print dependencies as JSON"
           listDepsJsonParser
      )
  <|> toListDepsOptsParser listDepsTextParser
