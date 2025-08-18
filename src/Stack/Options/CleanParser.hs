{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ApplicativeDo     #-}

{-|
Module      : Stack.Options.CleanParser
License     : BSD-3-Clause
-}

module Stack.Options.CleanParser
  ( cleanOptsParser
  ) where

import           Options.Applicative ( Parser, flag', help, idm, long, metavar )
import           Options.Applicative.Builder.Extra ( boolFlags )
import           Stack.Clean
                   ( CleanCommand (..), CleanDepth (..), CleanOpts (..) )
import           Stack.Prelude
import           Stack.Types.PackageName ( packageNameArgument )

-- | Command-line parser for the clean command.
cleanOptsParser :: CleanCommand -> Parser CleanOpts
cleanOptsParser Clean = shallowParser <|> fullParser

cleanOptsParser Purge = pure $ CleanOpts
  { depth = CleanFull
  , omitThis = False
  }

shallowParser :: Parser CleanOpts
shallowParser = do
  packages <- parsePackages
  omitThis <- parseOmitThis
  pure $ CleanOpts
    { depth = CleanShallow packages
    , omitThis
    }
 where
  parsePackages = many (packageNameArgument
    (  metavar "PACKAGE"
    <> help "If none specified, clean all project packages."
    ))
  parseOmitThis = boolFlags False
    "omit-this"
    "the omission of directories currently in use"
    idm

fullParser :: Parser CleanOpts
fullParser = do
  depth <- doFullClean
  pure $ CleanOpts
    { depth
    , omitThis = False
    }
 where
  doFullClean = flag' CleanFull
    (  long "full"
    <> help "Delete the project's Stack work directories (.stack-work by \
            \default)."
    )
