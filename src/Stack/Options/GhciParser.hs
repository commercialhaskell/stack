module Stack.Options.GhciParser where

import           Data.Monoid.Extra
import           Data.Version                      (showVersion)
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Paths_stack                       as Meta
import           Stack.Config                      (packagesParser)
import           Stack.Ghci                        (GhciOpts (..))

-- | Parser for GHCI options
ghciOptsParser :: Parser GhciOpts
ghciOptsParser = GhciOpts
             <$> many
                   (textArgument
                        (metavar "TARGET/FILE" <>
                         help ("If none specified, use all local packages. " <>
                               "See https://docs.haskellstack.org/en/v" <>
                               showVersion Meta.version <>
                               "/build_command/#target-syntax for details. " <>
                               "If a path to a .hs or .lhs file is specified, it will be loaded.")))
             <*> fmap concat (many (argsOption (long "ghci-options" <>
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
             -- TODO: deprecate this? probably useless.
             <*> switch (long "skip-intermediate-deps" <> help "Skip loading intermediate target dependencies" <> internal)
             <*> boolFlags True "package-hiding" "package hiding" idm
             <*> switch (long "no-build" <> help "Don't build before launching GHCi" <> internal)
