{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Options.CleanParser where

import           Options.Applicative
import           Stack.Clean                       (CleanCommand(..), CleanOpts (..))
import           Stack.Prelude
import           Stack.Types.PackageName

-- | Command-line parser for the clean command.
cleanOptsParser :: CleanCommand -> Parser CleanOpts
cleanOptsParser Clean = CleanShallow <$> packages <|> doFullClean
  where
    packages =
        many
            (packageNameArgument
                 (metavar "PACKAGE" <>
                  help "If none specified, clean all local packages"))
    doFullClean =
        flag'
            CleanFull
            (long "full" <>
             help "Delete all work directories (.stack-work by default) in the project")

cleanOptsParser Purge = pure CleanFull
