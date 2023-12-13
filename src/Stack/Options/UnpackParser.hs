{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @unpack@ command.
module Stack.Options.UnpackParser
  ( unpackOptsParser
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, ReadM, argument, eitherReader, help, long, metavar
                   , option, switch
                   )
import           Path ( SomeBase (..), parseSomeDir )
import           Stack.Prelude
import           Stack.Unpack ( UnpackOpts (..), UnpackTarget)

-- | Parse command line arguments for Stack's @unpack@ command.
unpackOptsParser :: Parser UnpackOpts
unpackOptsParser = UnpackOpts
  <$> some unpackTargetParser
  <*> areCandidatesParser
  <*> optional dirParser

unpackTargetParser :: Parser UnpackTarget
unpackTargetParser = argument unpackTargetReader
  (  metavar "TARGET"
  <> help "A package or package candidate (can be specified multiple times). A \
          \package can be referred to by name only or by identifier \
          \(including, optionally, a revision as '@rev:<number>' or \
          \'@sha256:<sha>'). A package candidate is referred to by its \
          \identifier."
  )

unpackTargetReader :: ReadM UnpackTarget
unpackTargetReader = eitherReader $ \s ->
  case parsePackageIdentifierRevision $ T.pack s of
    Right pir -> Right (Right pir)
    Left _ -> case parsePackageName s of
      Just pn -> Right (Left pn)
      Nothing ->
        Left $ s <> " is an invalid way to refer to a package or package \
                    \candidate to be unpacked."

areCandidatesParser :: Parser Bool
areCandidatesParser = switch
  (  long "candidate"
  <> help "Each target is a package candidate."
  )

dirParser :: Parser (SomeBase Dir)
dirParser = option dirReader
  (  long "to"
  <> metavar "DIR"
  <> help "Optionally, a directory to unpack into. A target will be unpacked \
          \ into a subdirectory."
  )

dirReader :: ReadM (SomeBase Dir)
dirReader = eitherReader $ \s ->
  case parseSomeDir s of
    Just dir -> Right dir
    Nothing ->
      Left $ s <> " is an invalid way to refer to a directory."
