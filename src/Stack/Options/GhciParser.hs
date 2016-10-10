module Stack.Options.GhciParser where

import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Stack.Config                      (packagesParser)
import           Stack.Ghci                        (GhciOpts (..))
import           Stack.Options.BuildParser
import           Stack.Types.Config

-- | Parser for GHCI options
ghciOptsParser :: Parser GhciOpts
ghciOptsParser = GhciOpts
             <$> fmap concat (many (argsOption (long "ghci-options" <>
                                       metavar "OPTION" <>
                                       help "Additional options passed to GHCi")))
             <*> optional
                     (strOption (long "with-ghc" <>
                                 metavar "GHC" <>
                                 help "Use this GHC to run GHCi"))
             <*> (not <$> boolFlags True "load" "load modules on start-up" idm)
             <*> packagesParser
             <*> optional
                     (textOption
                           (long "main-is" <>
                            metavar "TARGET" <>
                            help "Specify which target should contain the main \
                                 \module to load, such as for an executable for \
                                 \test suite or benchmark."))
             <*> switch (long "load-local-deps" <> help "Load all local dependencies of your targets")
             <*> switch (long "skip-intermediate-deps" <> help "Skip loading intermediate target dependencies")
             <*> boolFlags True "package-hiding" "package hiding" idm
             <*> buildOptsParser Build
             <*> switch (long "no-build" <> help "Don't build before launching GHCi (deprecated, should be unneeded)")
