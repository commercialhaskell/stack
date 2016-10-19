{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.BuildParser where

import qualified Data.Map as Map
import           Data.Monoid.Extra
import           Data.Text (Text)
import           Data.Version (showVersion)
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Paths_stack as Meta
import           Stack.Options.PackageParser (readFlag)
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.PackageName

-- | Parser for CLI-only build arguments
buildOptsParser :: BuildCommand
                -> Parser BuildOptsCLI
buildOptsParser cmd =
    BuildOptsCLI <$>
    targetsParser <*>
    switch
        (long "dry-run" <>
         help "Don't build anything, just prepare to") <*>
    ((\x y z ->
           concat [x, y, z]) <$>
     flag
         []
         ["-Wall", "-Werror"]
         (long "pedantic" <>
          help "Turn on -Wall and -Werror") <*>
     flag
         []
         ["-O0"]
         (long "fast" <>
          help "Turn off optimizations (-O0)") <*>
     many
         (textOption
              (long "ghc-options" <>
               metavar "OPTION" <>
               help "Additional options passed to GHC"))) <*>
    flagsParser <*>
    (flag'
         BSOnlyDependencies
         (long "dependencies-only" <>
          help "A synonym for --only-dependencies") <|>
     flag'
         BSOnlySnapshot
         (long "only-snapshot" <>
          help
              "Only build packages for the snapshot database, not the local database") <|>
     flag'
         BSOnlyDependencies
         (long "only-dependencies" <>
          help
              "Only build packages that are dependencies of targets on the command line") <|>
     pure BSAll) <*>
    (flag'
         FileWatch
         (long "file-watch" <>
          help
              "Watch for changes in local files and automatically rebuild. Ignores files in VCS boring/ignore file") <|>
     flag'
         FileWatchPoll
         (long "file-watch-poll" <>
          help
              "Like --file-watch, but polling the filesystem instead of using events") <|>
     pure NoFileWatch) <*>
    many (cmdOption
             (long "exec" <>
              metavar "CMD [ARGS]" <>
              help "Command and arguments to run after a successful build")) <*>
    switch
        (long "only-configure" <>
         help
             "Only perform the configure step, not any builds. Intended for tool usage, may break when used on multiple packages at once!") <*>
    pure cmd <*>
    switch
        (long "initial-build-steps" <>
         help "For target packages, only run initial build steps needed for GHCi" <>
         internal)

targetsParser :: Parser [Text]
targetsParser =
    many
        (textArgument
             (metavar "TARGET" <>
              help ("If none specified, use all local packages. " <>
                    "See https://docs.haskellstack.org/en/v" <>
                    showVersion Meta.version <>
                    "/build_command/#target-syntax for details.")))

flagsParser :: Parser (Map.Map (Maybe PackageName) (Map.Map FlagName Bool))
flagsParser =
     Map.unionsWith Map.union <$>
     many
         (option
              readFlag
              (long "flag" <>
               metavar "PACKAGE:[-]FLAG" <>
               help
                   ("Override flags set in stack.yaml " <>
                    "(applies to local packages and extra-deps)")))
