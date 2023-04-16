{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @init@ and @new@
-- commands.
module Stack.Options.InitParser
  ( initOptsParser
  ) where

import           Options.Applicative
                   ( Parser, completer, help, long, metavar, switch )
import           Options.Applicative.Builder.Extra
                   ( dirCompleter, textArgument )
import           Stack.Init ( InitOpts (..) )
import           Stack.Prelude

-- | Parse command line arguments for Stack's @init@ and @new@ commands.
initOptsParser :: Parser InitOpts
initOptsParser = InitOpts
  <$> searchDirs
  <*> omitPackages
  <*> overwrite
  <*> fmap not ignoreSubDirs
 where
  searchDirs = many (textArgument
    (  metavar "DIR(S)"
    <> completer dirCompleter
    <> help "Directory, or directories, to include in the search for .cabal \
            \files, when initialising. The default is the current directory."
    ))
  ignoreSubDirs = switch
    (  long "ignore-subdirs"
    <> help "Do not search for .cabal files in subdirectories, when \
            \initialising."
    )
  overwrite = switch
    (  long "force"
    <> help "Force an initialisation that overwrites any existing stack.yaml \
            \file."
    )
  omitPackages = switch
    (  long "omit-packages"
    <> help "Exclude conflicting or incompatible user packages, when \
            \initialising."
    )
