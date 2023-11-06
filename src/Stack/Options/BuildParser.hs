{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.BuildParser
  ( buildOptsParser
  , flagsParser
  , targetsParser
  ) where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, completer, flag, flag', help, internal, long
                   , metavar, option, strOption, switch, value
                   )
import           Options.Applicative.Args ( cmdOption )
import           Options.Applicative.Builder.Extra ( textArgument, textOption )
import           Stack.Options.Completion
                   ( flagCompleter, ghcOptsCompleter, targetCompleter )
import           Stack.Options.PackageParser ( readFlag )
import           Stack.Prelude
import           Stack.Types.BuildOpts
                   ( ApplyCLIFlag, BuildCommand, BuildOptsCLI (..)
                   , BuildSubset (..), FileWatchOpts (..)
                   )

-- | Parser for CLI-only build arguments
buildOptsParser :: BuildCommand -> Parser BuildOptsCLI
buildOptsParser cmd = BuildOptsCLI
  <$> targetsParser
  <*> switch
        (  long "dry-run"
        <> help "Don't build anything, just prepare to."
        )
  <*> (   (\x y z -> concat [x, y, z])
      <$> flag
            []
            ["-Wall", "-Werror"]
            (  long "pedantic"
            <> help "Turn on -Wall and -Werror."
            )
      <*> flag
            []
            ["-O0"]
            (  long "fast"
            <> help "Turn off optimizations (-O0)."
            )
      <*> many (textOption
            (  long "ghc-options"
            <> metavar "OPTIONS"
            <> completer ghcOptsCompleter
            <> help "Additional options passed to GHC (can be specified \
                    \multiple times)."
            ))
      )
  <*> progsOptionsParser
  <*> flagsParser
  <*> (   flag' BSOnlyDependencies
            (  long "dependencies-only"
            <> help "A synonym for --only-dependencies."
            )
      <|> flag' BSOnlySnapshot
            (  long "only-snapshot"
            <> help "Only build packages for the snapshot database, not the \
                    \local database."
            )
      <|> flag' BSOnlyDependencies
            (  long "only-dependencies"
            <> help "Only build packages that are dependencies of targets on \
                    \the command line."
            )
      <|> flag' BSOnlyLocals
            (  long "only-locals"
            <> help "Only build packages in the local database. Fail if the \
                    \build plan includes the snapshot database."
            )
      <|> pure BSAll
      )
  <*> (   flag' FileWatch
            (  long "file-watch"
            <> help "Watch for changes in local files and automatically \
                    \rebuild."
            )
      <|> flag' FileWatchPoll
            (  long "file-watch-poll"
            <> help "Like --file-watch, but polling the filesystem instead of \
                    \using events."
            )
      <|> pure NoFileWatch
      )
  <*> switch
        (  long "watch-all"
        <> help "Watch all local files not taking targets into account."
        )
  <*> many (cmdOption
        (  long "exec"
        <> metavar "COMMAND [ARGUMENT(S)]"
        <> help "Command and argument(s) to run after a successful build."
        ))
  <*> switch
        (  long "only-configure"
        <> help "Only perform the configure step, not any builds. Intended for \
                \tool usage. May break when used on multiple packages at once!"
        )
  <*> pure cmd
  <*> switch
        (  long "initial-build-steps"
        <> help "For target packages, only run initial build steps needed for \
                \GHCi."
        <> internal
        )

targetsParser :: Parser [Text]
targetsParser =
  many (textArgument
    (  metavar "TARGET"
    <> completer targetCompleter
    <> help "If none specified, use all local packages. See \
            \https://docs.haskellstack.org/en/stable/build_command/#target-syntax \
            \for details."
    ))

flagsParser :: Parser (Map.Map ApplyCLIFlag (Map.Map FlagName Bool))
flagsParser = Map.unionsWith Map.union
  <$> many (option readFlag
       (  long "flag"
       <> completer flagCompleter
       <> metavar "PACKAGE:[-]FLAG"
       <> help "Override flags set in stack.yaml (applies to local packages \
               \and extra-deps)."
       ))

progsOptionsParser :: Parser [(Text, [Text])]
progsOptionsParser =
     dummyProgOptionsParser
  *> (filter (not . L.null . snd) <$> progsOptionsParser')
 where
  -- The purpose of this parser is only to generate the desired help text. The
  -- actual --PROG-options parsers are all internal.
  dummyProgOptionsParser :: Parser String
  dummyProgOptionsParser = strOption
    ( long "PROG-option"
    <> help
         (  "Pass an argument to PROG (can be specified multiple times). PROG \
            \must be a program recognised by the Cabal library and one of "
         <> T.unpack (T.intercalate " " progs) <> "."
         )
    <> metavar "ARG"
    <> value ""
    )
  progs :: [Text]
  progs = L.sort
    [
      -- configuration
      "pkg-config"
      -- preprocessors
    , "alex"
    , "c2hs"
    , "cpphs"
    , "doctest"
    , "greencard"
    , "happy"
    , "hsc2hs"
    , "hscolour"
      -- platform toolchain (GNU)
    , "ar"  -- create, modify, and extract from archives
    , "gcc" -- C/C++ compiler
    , "ld" -- linker
    , "strip" -- discards symbols and other data from object files
    , "tar"
    ]
  progsOptionsParser' :: Parser [(Text, [Text])]
  progsOptionsParser' = traverse mkProgOptionsParser progs
  mkProgOptionsParser :: Text -> Parser (Text, [Text])
  mkProgOptionsParser prog = fmap (prog,) $ many $ textOption
    (  long (T.unpack prog <> "-option")
    <> internal
    )
