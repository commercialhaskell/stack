{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @upgrade@ command.
module Stack.Options.UpgradeParser
  ( upgradeOptsParser
  ) where

import         Options.Applicative
                 ( Parser, flag', help, long, showDefault, strOption, switch
                 , value
                 )
import         Stack.Prelude
import         Stack.Upgrade
                 ( BinaryOpts (..), SourceOpts (..), UpgradeOpts (..) )

-- | Parse command line arguments for Stack's @upgrade@ command.
upgradeOptsParser :: Parser UpgradeOpts
upgradeOptsParser = UpgradeOpts
  <$> (sourceOnly <|> optional binaryOpts)
  <*> (binaryOnly <|> optional sourceOpts)
 where
  binaryOnly = flag' Nothing
    (  long "binary-only"
    <> help "Do not use a source upgrade path"
    )
  sourceOnly = flag' Nothing
    (  long "source-only"
    <> help "Do not use a binary upgrade path"
    )

  binaryOpts = BinaryOpts
    <$> optional (strOption
          (  long "binary-platform"
          <> help "Platform type for archive to download"
          ))
    <*> switch
          (  long "force-download"
          <> help "Download the latest available Stack executable"
          )
    <*> optional (strOption
          (  long "binary-version"
          <> help "Download a specific Stack version"
          ))
    <*> optional (strOption
          (  long "github-org"
          <> help "GitHub organization name"
          ))
    <*> optional (strOption
          (  long "github-repo"
          <> help "GitHub repository name"
          ))

  sourceOpts = SourceOpts
    <$> (   ( \fromGit repo branch ->
                if fromGit
                  then Just (repo, branch)
                  else Nothing
            )
        <$> switch
              ( long "git"
              <> help "Clone from Git instead of downloading from Hackage \
                      \(more dangerous)"
              )
        <*> strOption
              (  long "git-repo"
              <> help "Clone from specified git repository"
              <> value "https://github.com/commercialhaskell/stack"
              <> showDefault
              )
        <*> strOption
              (  long "git-branch"
              <> help "Clone from this git branch"
              <> value "master"
              <> showDefault
              )
        )
