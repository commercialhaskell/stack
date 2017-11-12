{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Options.HpcReportParser where

import qualified Data.Text                         as T
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Types         (readerAsk)
import           Stack.Coverage                    (HpcReportOpts (..))
import           Stack.Options.Completion          (targetCompleter)
import           Stack.Prelude
import           Stack.Types.Config

-- | Parser for @stack hpc report@.
hpcReportOptsParser :: Parser HpcReportOpts
hpcReportOptsParser = HpcReportOpts
    <$> many (textArgument $ metavar "TARGET_OR_TIX" <>
                             completer (targetCompleter <> fileExtCompleter [".tix"]))
    <*> switch (long "all" <> help "Use results from all packages and components involved in previous --coverage run")
    <*> optional (strOption (long "destdir" <>
                             metavar "DIR" <>
                             completer dirCompleter <>
                             help "Output directory for HTML report"))
    <*> switch (long "open" <> help "Open the report in the browser")

pvpBoundsOption :: Parser PvpBounds
pvpBoundsOption =
    option
        readPvpBounds
        (long "pvp-bounds" <>
         metavar "PVP-BOUNDS" <>
         completeWith ["none", "lower", "upper", "both"] <>
         help
             "How PVP version bounds should be added to .cabal file: none, lower, upper, both")
  where
    readPvpBounds = do
        s <- readerAsk
        case parsePvpBounds $ T.pack s of
            Left e ->
                readerError e
            Right v ->
                return v
