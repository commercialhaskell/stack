{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.GhcBuildParser where

import           Options.Applicative
import           Options.Applicative.Types
import           Stack.Options.Utils
import           Stack.Prelude
import           Stack.Types.CompilerBuild

-- | GHC build parser
ghcBuildParser :: Bool -> Parser CompilerBuild
ghcBuildParser hide =
    option
        readGHCBuild
        (long "ghc-build" <> metavar "BUILD" <>
         completeWith [ "standard"
                      , "gmp4"
                      , "nopie"
                      , "tinfo6"
                      , "tinfo6-nopie"
                      , "ncurses6"
                      , "int-native"
                      , "integersimple"] <>
         help
             "Specialized GHC build, e.g. 'gmp4' or 'standard' (usually auto-detected)" <>
         hideMods hide
        )
  where
    readGHCBuild = do
        s <- readerAsk
        case parseCompilerBuild s of
            Left e -> readerError (displayException e)
            Right v -> pure v
