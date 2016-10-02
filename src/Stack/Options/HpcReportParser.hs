module Stack.Options.HpcReportParser where

import           Data.Monoid.Extra
import qualified Data.Text                         as T
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Types         (readerAsk)
import           Stack.Coverage                    (HpcReportOpts (..))
import           Stack.Types.Config

-- | Parser for @stack hpc report@.
hpcReportOptsParser :: Parser HpcReportOpts
hpcReportOptsParser = HpcReportOpts
    <$> many (textArgument $ metavar "TARGET_OR_TIX")
    <*> switch (long "all" <> help "Use results from all packages and components involved in previous --coverage run")
    <*> optional (strOption (long "destdir" <> help "Output directory for HTML report"))
    <*> switch (long "open" <> help "Open the report in the browser")

pvpBoundsOption :: Parser PvpBounds
pvpBoundsOption =
    option
        readPvpBounds
        (long "pvp-bounds" <> metavar "PVP-BOUNDS" <>
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
