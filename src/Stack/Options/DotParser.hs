{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.DotParser where

import           Data.Char (isSpace)
import           Data.List.Split (splitOn)
import           Data.Monoid.Extra
import qualified Data.Set as Set
import qualified Data.Text as T
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Dot
import           Stack.Options.BuildParser

-- | Parser for arguments to `stack dot`
dotOptsParser :: Bool -> Parser DotOpts
dotOptsParser externalDefault =
  DotOpts <$> includeExternal
          <*> includeBase
          <*> depthLimit
          <*> fmap (maybe Set.empty Set.fromList . fmap splitNames) prunedPkgs
          <*> targetsParser
          <*> flagsParser
          <*> testTargets
          <*> benchTargets
  where includeExternal = boolFlags externalDefault
                                    "external"
                                    "inclusion of external dependencies"
                                    idm
        includeBase = boolFlags True
                                "include-base"
                                "inclusion of dependencies on base"
                                idm
        depthLimit =
            optional (option auto
                             (long "depth" <>
                              metavar "DEPTH" <>
                              help ("Limit the depth of dependency resolution " <>
                                    "(Default: No limit)")))
        prunedPkgs = optional (strOption
                                   (long "prune" <>
                                    metavar "PACKAGES" <>
                                    help ("Prune each package name " <>
                                          "from the comma separated list " <>
                                          "of package names PACKAGES")))
        testTargets = switch (long "test" <>
                              help "Consider dependencies of test components")
        benchTargets = switch (long "bench" <>
                               help "Consider dependencies of benchmark components")

        splitNames :: String -> [String]
        splitNames = map (takeWhile (not . isSpace) . dropWhile isSpace) . splitOn ","

-- | Parser for arguments to `stack list-dependencies`.
listDepsOptsParser :: Parser ListDepsOpts
listDepsOptsParser = ListDepsOpts
                 <$> dotOptsParser True -- Default for --external is True.
                 <*> fmap escapeSep
                     (textOption (long "separator" <>
                                  metavar "SEP" <>
                                  help ("Separator between package name " <>
                                        "and package version.") <>
                                  value " " <>
                                  showDefault))
                 <*> boolFlags False
                                "license"
                                "printing of dependency licenses instead of versions"
                                idm
  where escapeSep sep = T.replace "\\t" "\t" (T.replace "\\n" "\n" sep)
