{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Options.IdeParser
Description : Parse arguments for Stack's @ide@ commands.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @ide@ commands.
-}

module Stack.Options.IdeParser
  ( idePackagesParser
  , ideTargetsParser
  ) where

import           Options.Applicative ( Parser, flag, help, long, switch )
import           Stack.Prelude
import           Stack.Types.IdeOpts ( ListPackagesCmd (..), OutputStream (..) )

-- | Parse command line arguments for Stack's @ide packages@ command.
idePackagesParser :: Parser (OutputStream, ListPackagesCmd)
idePackagesParser = (,) <$> outputFlag <*> cabalFileFlag

-- | Parse command line arguments for Stack's @ide targets@ command.
ideTargetsParser :: Parser ((Bool, Bool, Bool), OutputStream)
ideTargetsParser =
  (,) <$> ((,,) <$> exeFlag <*> testFlag <*> benchFlag) <*> outputFlag

outputFlag :: Parser OutputStream
outputFlag = flag
  OutputLogInfo
  OutputStdout
  (  long "stdout"
  <> help "Send output to the standard output stream instead of the \
          \default, the standard error stream."
  )

cabalFileFlag :: Parser ListPackagesCmd
cabalFileFlag = flag
  ListPackageNames
  ListPackageCabalFiles
  (  long "cabal-files"
  <> help "Print paths to package Cabal files instead of package \
          \names."
  )

exeFlag :: Parser Bool
exeFlag = switch
  (  long "exes"
  <> help "Include executables."
  )

testFlag :: Parser Bool
testFlag = switch
  (  long "tests"
  <> help "Include test suites."
  )

benchFlag :: Parser Bool
benchFlag = switch
  (  long "benchmarks"
  <> help "Include benchmarks."
  )
