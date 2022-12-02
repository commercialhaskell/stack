{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.ExecParser where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Args
import           Stack.Options.Completion
import           Stack.Prelude
import           Stack.Types.Config

-- | Parser for exec command
execOptsParser :: Maybe SpecialExecCmd -> Parser ExecOpts
execOptsParser mcmd =
    ExecOpts
        <$> maybe eoCmdParser pure mcmd
        <*> eoArgsParser
        <*> execOptsExtraParser
  where
    eoCmdParser = ExecCmd <$> strArgument (metavar "COMMAND" <> completer projectExeCompleter)
    eoArgsParser = many (strArgument (metavar txt))
      where
        txt = case mcmd of
            Nothing -> normalTxt
            Just ExecCmd{} -> normalTxt
            Just ExecRun -> "-- ARGUMENT(S) (e.g. stack run -- file.txt)"
            Just ExecGhc -> "-- ARGUMENT(S) (e.g. stack ghc -- X.hs -o x)"
            Just ExecRunGhc -> "-- ARGUMENT(S) (e.g. stack runghc -- X.hs)"
        normalTxt = "-- ARGUMENT(S) (e.g. stack exec ghc-pkg -- describe base)"

evalOptsParser :: String -- ^ metavar
               -> Parser EvalOpts
evalOptsParser meta =
    EvalOpts
        <$> eoArgsParser
        <*> execOptsExtraParser
  where
    eoArgsParser :: Parser String
    eoArgsParser = strArgument (metavar meta)

-- | Parser for extra options to exec command
execOptsExtraParser :: Parser ExecOptsExtra
execOptsExtraParser = ExecOptsExtra
                         <$> eoEnvSettingsParser
                         <*> eoPackagesParser
                         <*> eoRtsOptionsParser
                         <*> eoCwdParser
  where
    eoEnvSettingsParser :: Parser EnvSettings
    eoEnvSettingsParser = EnvSettings True
        <$> boolFlags True
                "ghc-package-path"
                "setting the GHC_PACKAGE_PATH variable for the subprocess"
                idm
        <*> boolFlags True
                "stack-exe"
                "setting the STACK_EXE environment variable to the path for the stack executable"
                idm
        <*> pure False
        <*> pure True

    eoPackagesParser :: Parser [String]
    eoPackagesParser = many
                       (strOption (long "package"
                                  <> metavar "PACKAGE"
                                  <> help "Add a package (can be specified multiple times)"))

    eoRtsOptionsParser :: Parser [String]
    eoRtsOptionsParser = concat <$> many (argsOption
        ( long "rts-options"
        <> help "Explicit RTS options to pass to application"
        <> metavar "RTSFLAG"))

    eoCwdParser :: Parser (Maybe FilePath)
    eoCwdParser = optional
                  (strOption (long "cwd"
                             <> help "Sets the working directory before executing"
                             <> metavar "DIR"
                             <> completer dirCompleter)
                  )
