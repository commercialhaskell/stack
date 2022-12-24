{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.GhcBuildParser
( ghcBuildParser
) where

import           Options.Applicative
                   ( Parser, completeWith, help, long, metavar, option )
import           Options.Applicative.Types ( readerAsk, readerError )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude
import           Stack.Types.CompilerBuild ( CompilerBuild, parseCompilerBuild )

-- | GHC build parser
ghcBuildParser :: Bool -> Parser CompilerBuild
ghcBuildParser hide = option readGHCBuild
  (  long "ghc-build"
  <> metavar "BUILD"
  <> completeWith
       [ "standard"
       , "gmp4"
       , "nopie"
       , "tinfo6"
       , "tinfo6-libc6-pre232"
       , "tinfo6-nopie"
       , "ncurses6"
       , "int-native"
       , "integersimple"
       ]
  <> help "Specialized GHC build, e.g. 'gmp4' or 'standard' (usually \
          \auto-detected)"
  <> hideMods hide
  )
 where
  readGHCBuild = do
    s <- readerAsk
    case parseCompilerBuild s of
      Left e -> readerError (displayException e)
      Right v -> pure v
