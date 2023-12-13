{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @unpack@ command.
module Stack.Options.UnpackParser
  ( unpackOptsParser
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, ReadM, argument, eitherReader, help, long, metavar
                   , option
                   )
import           Path ( SomeBase (..), parseSomeDir )
import           Stack.Prelude
import           Stack.Unpack ( UnpackOpts (..), UnpackTarget)

-- | Parse command line arguments for Stack's @unpack@ command.
unpackOptsParser :: Parser UnpackOpts
unpackOptsParser = UnpackOpts
  <$> some unpackTargetParser
  <*> optional dirParser

unpackTargetParser :: Parser UnpackTarget
unpackTargetParser = argument unpackTargetReader
  (  metavar "PACKAGE"
  <> help "A package, referred to by name only or by identifier (including, \
          \optionally, a Hackage revision as '@rev:<number>' or \
          \'@sha256:<sha>') (can be specified multiple times)."
  )

unpackTargetReader :: ReadM UnpackTarget
unpackTargetReader = eitherReader $ \s ->
  case parsePackageIdentifierRevision $ T.pack s of
    Right pir -> Right (Right pir)
    Left _ -> case parsePackageName s of
      Just pn -> Right (Left pn)
      Nothing ->
        Left $ s <> " is an invalid way to refer to a package to be unpacked."

dirParser :: Parser (SomeBase Dir)
dirParser = option dirReader
  (  long "to"
  <> metavar "DIR"
  <> help "Optionally, a directory to unpack into. A package will be unpacked \
          \ into a subdirectory."
  )

dirReader :: ReadM (SomeBase Dir)
dirReader = eitherReader $ \s ->
  case parseSomeDir s of
    Just dir -> Right dir
    Nothing ->
      Left $ s <> " is an invalid way to refer to a directory."
