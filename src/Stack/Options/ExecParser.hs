module Stack.Options.ExecParser where

import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Types.Config

-- | Parser for exec command
execOptsParser :: Maybe SpecialExecCmd -> Parser ExecOpts
execOptsParser mcmd =
    ExecOpts
        <$> maybe eoCmdParser pure mcmd
        <*> eoArgsParser
        <*> execOptsExtraParser
  where
    eoCmdParser = ExecCmd <$> strArgument (metavar "CMD")
    eoArgsParser = many (strArgument (metavar "-- ARGS (e.g. stack ghc -- X.hs -o x)"))

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
execOptsExtraParser = eoPlainParser <|>
                      ExecOptsEmbellished
                         <$> eoEnvSettingsParser
                         <*> eoPackagesParser
  where
    eoEnvSettingsParser :: Parser EnvSettings
    eoEnvSettingsParser = EnvSettings
        <$> pure True
        <*> boolFlags True
                "ghc-package-path"
                "setting the GHC_PACKAGE_PATH variable for the subprocess"
                idm
        <*> boolFlags True
                "stack-exe"
                "setting the STACK_EXE environment variable to the path for the stack executable"
                idm
        <*> pure False

    eoPackagesParser :: Parser [String]
    eoPackagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))

    eoPlainParser :: Parser ExecOptsExtra
    eoPlainParser = flag' ExecOptsPlain
                          (long "plain" <>
                           help "Use an unmodified environment (only useful with Docker)")

