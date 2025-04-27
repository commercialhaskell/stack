{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.UpgradeParser
Description : Parse arguments for Stack's @upgrade@ command.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @upgrade@ command.
-}

module Stack.Options.UpgradeParser
  ( upgradeOptsParser
  ) where

import         Options.Applicative
                 ( Parser, flag', help, idm, long, metavar, showDefault
                 , strOption, switch, value
                 )
import         Options.Applicative.Builder.Extra ( boolFlags )
import         Stack.Prelude
import         Stack.Upgrade
                 ( BinaryOpts (..), SourceOpts (..), UpgradeOpts (..) )

-- | Parse command line arguments for Stack's @upgrade@ command.
upgradeOptsParser ::
     Bool
     -- ^ The default for --[no]-only-local-bin
  -> Parser UpgradeOpts
upgradeOptsParser onlyLocalBin = UpgradeOpts
  <$> (sourceOnly <|> optional binaryOpts)
  <*> (binaryOnly <|> optional sourceOpts)
 where
  binaryOnly = flag' Nothing
    (  long "binary-only"
    <> help "Do not use a source upgrade path."
    )
  sourceOnly = flag' Nothing
    (  long "source-only"
    <> help "Do not use a binary upgrade path."
    )

  binaryOpts = BinaryOpts
    <$> optional (strOption
          (  long "binary-platform"
          <> help "Platform type for archive to download."
          <> metavar "PLATFORM"
          ))
    <*> switch
          (  long "force-download"
          <> help "Download the latest available Stack executable, even if not \
                  \newer."
          )
    <*> boolFlags onlyLocalBin
          "only-local-bin"
          "downloading only to Stack's local binary directory"
          idm
    <*> optional (strOption
          (  long "binary-version"
          <> help "Download a specific Stack version, even if already \
                  \installed."
          <> metavar "VERSION"
          ))
    <*> optional (strOption
          (  long "github-org"
          <> help "GitHub organization name."
          <> metavar "USER"
          ))
    <*> optional (strOption
          (  long "github-repo"
          <> help "GitHub repository name."
          <> metavar "REPO"
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
                      \(more dangerous)."
              )
        <*> strOption
              (  long "git-repo"
              <> help "Clone from specified Git repository."
              <> metavar "URL"
              <> value "https://github.com/commercialhaskell/stack"
              <> showDefault
              )
        <*> strOption
              (  long "git-branch"
              <> help "Clone from specified Git branch."
              <> metavar "BRANCH"
              <> value "master"
              <> showDefault
              )
        )
