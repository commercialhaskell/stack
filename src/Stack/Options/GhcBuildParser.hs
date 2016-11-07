module Stack.Options.GhcBuildParser where

import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Types
import           Stack.Options.Utils
import           Stack.Types.CompilerBuild

-- | GHC build parser
ghcBuildParser :: Bool -> Parser CompilerBuild
ghcBuildParser hide =
    option
        readGHCBuild
        (long "ghc-build" <> metavar "BUILD" <>
         help
             "Specialized GHC build, e.g. 'gmp4' or 'standard' (usually auto-detected)" <>
         hideMods hide
        )
  where
    readGHCBuild = do
        s <- readerAsk
        case parseCompilerBuild s of
            Left e -> readerError (show e)
            Right v -> return v
