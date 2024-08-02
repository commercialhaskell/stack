{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Function to parse command line arguments for Stack's @ls@ command.
module Stack.Options.LsParser
  ( lsOptsParser
  ) where

import qualified Data.Text as T
import qualified Options.Applicative as OA
import           Options.Applicative ( idm )
import           Options.Applicative.Builder.Extra ( boolFlags, textOption )
import           Stack.Constants ( globalFooter )
import           Stack.Ls
                   ( ListDepsFormat (..), ListDepsFormatOpts (..)
                   , ListDepsOpts (..), ListDepsTextFilter (..)
                   , ListGlobalsOpts (..), ListStylesOpts (..)
                   , ListToolsOpts (..), LsCmdOpts (..), LsCmds (..)
                   , LsView (..), SnapshotOpts (..), ListGlobalsOpts
                   )
import           Stack.Options.DotParser ( dotOptsParser )
import           Stack.Prelude

-- | Parse command line arguments for Stack's @ls@ command.
lsOptsParser :: OA.Parser LsCmdOpts
lsOptsParser = LsCmdOpts
  <$> OA.hsubparser
        (  lsSnapCmd
        <> lsGlobalsCmd
        <> lsDepsCmd
        <> lsStylesCmd
        <> lsToolsCmd
        )

lsSnapCmd :: OA.Mod OA.CommandFields LsCmds
lsSnapCmd = OA.command "snapshots" $
  OA.info lsCmdOptsParser $
       OA.progDesc "View snapshots. (default: local)"
    <> OA.footer localSnapshotMsg

lsGlobalsCmd :: OA.Mod OA.CommandFields LsCmds
lsGlobalsCmd = OA.command "globals" $
  OA.info lsGlobalsOptsParser $
       OA.progDesc "View global packages."
    <> OA.footer globalFooter

lsDepsCmd :: OA.Mod OA.CommandFields LsCmds
lsDepsCmd = OA.command "dependencies" $
  OA.info lsDepOptsParser $
       OA.progDesc
         "View the packages and versions used for a project. Use a command if \
         \the first target specified has the name of a command. Targets other \
         \than project packages are ignored."
    <> OA.footer globalFooter

lsStylesCmd :: OA.Mod OA.CommandFields LsCmds
lsStylesCmd =
     OA.command
       "stack-colors"
       (OA.info lsStylesOptsParser
                (OA.progDesc "View Stack's output styles."))
  <> OA.command
       "stack-colours"
       (OA.info lsStylesOptsParser
                (OA.progDesc "View Stack's output styles (alias for \
                             \'stack-colors')."))

lsToolsCmd :: OA.Mod OA.CommandFields LsCmds
lsToolsCmd =
  OA.command
    "tools"
    (OA.info lsToolsOptsParser
             (OA.progDesc "View Stack's installed tools."))

lsCmdOptsParser :: OA.Parser LsCmds
lsCmdOptsParser = LsSnapshot <$> lsViewSnapCmd

lsGlobalsOptsParser :: OA.Parser LsCmds
lsGlobalsOptsParser = LsGlobals <$> listGlobalsOptsParser

lsDepOptsParser :: OA.Parser LsCmds
lsDepOptsParser = LsDependencies <$> listDepsOptsParser

lsStylesOptsParser :: OA.Parser LsCmds
lsStylesOptsParser = LsStyles <$> listStylesOptsParser

lsToolsOptsParser :: OA.Parser LsCmds
lsToolsOptsParser = LsTools <$> listToolsOptsParser

lsViewSnapCmd :: OA.Parser SnapshotOpts
lsViewSnapCmd = SnapshotOpts
  <$> ( OA.hsubparser (lsViewRemoteCmd <> lsViewLocalCmd) <|> pure Local)
  <*> OA.switch
        (  OA.long "lts"
        <> OA.short 'l'
        <> OA.help "Only show LTS Haskell snapshots."
        )
  <*> OA.switch
        (  OA.long "nightly"
        <> OA.short 'n'
        <> OA.help "Only show Nightly snapshots."
        )

lsViewRemoteCmd :: OA.Mod OA.CommandFields LsView
lsViewRemoteCmd = OA.command "remote" $
  OA.info (pure Remote) $
       OA.progDesc "View remote snapshots."
    <> OA.footer pagerMsg

pagerMsg :: String
pagerMsg =
  "On a terminal, uses a pager, if one is available. Respects the PAGER \
  \environment variable (subject to that, prefers pager 'less' to 'more')."

lsViewLocalCmd :: OA.Mod OA.CommandFields LsView
lsViewLocalCmd = OA.command "local" $
  OA.info (pure Local) $
       OA.progDesc "View local snapshots."
    <> OA.footer localSnapshotMsg

localSnapshotMsg :: String
localSnapshotMsg =
  "A local snapshot is identified by a hash code. " <> pagerMsg

-- | Parser for arguments to `stack ls globals`.
listGlobalsOptsParser :: OA.Parser ListGlobalsOpts
listGlobalsOptsParser = ListGlobalsOpts <$> globalHints
 where
  globalHints = boolFlags True
    "global-hints"
    "use of a hints file for global packages, rather than an installed GHC"
    idm

-- | Parser for arguments to `stack ls dependencies`.
listDepsOptsParser :: OA.Parser ListDepsOpts
listDepsOptsParser = OA.subparser
      (  formatSubCommand
           "text"
           "Print dependencies as text (default)."
           listDepsTextParser
      <> formatSubCommand
           "cabal"
           "Print dependencies as exact Cabal constraints."
           listDepsConstraintsParser
      <> formatSubCommand
           "tree"
           "Print dependencies as tree."
           listDepsTreeParser
      <> formatSubCommand
           "json"
           "Print dependencies as JSON."
           listDepsJsonParser
      )
  <|> toListDepsOptsParser listDepsTextParser

formatSubCommand ::
     String
  -> String
  -> OA.Parser ListDepsFormat
  -> OA.Mod OA.CommandFields ListDepsOpts
formatSubCommand cmd desc formatParser =
  OA.command
    cmd (OA.info (toListDepsOptsParser formatParser) (OA.progDesc desc))

listDepsTextParser :: OA.Parser ListDepsFormat
listDepsTextParser =
  ListDepsText <$> listDepsFormatOptsParser <*> textFilterParser

textFilterParser :: OA.Parser [ListDepsTextFilter]
textFilterParser = many (OA.option parseListDepsTextFilter
  (  OA.long "filter"
  <> OA.metavar "ITEM"
  <> OA.help "Item to be filtered out of the results, if present, being either \
             \$locals (for all project packages) or a package name (can be \
             \specified multiple times)."
  ))

parseListDepsTextFilter :: OA.ReadM ListDepsTextFilter
parseListDepsTextFilter = OA.eitherReader $ \s ->
  if s == "$locals"
    then Right FilterLocals
    else case parsePackageName s of
      Just pkgName -> Right $ FilterPackage pkgName
      Nothing -> Left $ s <> " is not a valid package name."

listDepsConstraintsParser :: OA.Parser ListDepsFormat
listDepsConstraintsParser = pure ListDepsConstraints

listDepsTreeParser :: OA.Parser ListDepsFormat
listDepsTreeParser =  ListDepsTree <$> listDepsFormatOptsParser

listDepsJsonParser :: OA.Parser ListDepsFormat
listDepsJsonParser = pure ListDepsJSON

listDepsFormatOptsParser :: OA.Parser ListDepsFormatOpts
listDepsFormatOptsParser = ListDepsFormatOpts
  <$> separatorParser
  <*> licenseParser

separatorParser :: OA.Parser Text
separatorParser = fmap
  escapeSep
  ( textOption
      (  OA.long "separator"
      <> OA.metavar "SEP"
      <> OA.help "Separator between package name and package version."
      <> OA.value " "
      <> OA.showDefault
      )
  )
 where
  escapeSep s = T.replace "\\t" "\t" (T.replace "\\n" "\n" s)

licenseParser :: OA.Parser Bool
licenseParser = boolFlags False
  "license"
  "printing of dependency licenses instead of versions."
  idm

toListDepsOptsParser :: OA.Parser ListDepsFormat -> OA.Parser ListDepsOpts
toListDepsOptsParser formatParser = ListDepsOpts
  <$> formatParser
  <*> dotOptsParser True

listStylesOptsParser :: OA.Parser ListStylesOpts
listStylesOptsParser = ListStylesOpts
  <$> boolFlags False
        "basic"
        "a basic report of the styles used. The default is a fuller one."
        idm
  <*> boolFlags True
        "sgr"
        "the provision of the equivalent SGR instructions (provided by \
        \default). Flag ignored for a basic report."
        idm
  <*> boolFlags True
        "example"
        "the provision of an example of the applied style (provided by default \
        \for colored output). Flag ignored for a basic report."
        idm

listToolsOptsParser :: OA.Parser ListToolsOpts
listToolsOptsParser = ListToolsOpts
  <$> OA.strOption
        (  OA.long "filter"
        <> OA.metavar "TOOL_NAME"
        <> OA.value ""
        <> OA.help "Filter by a tool name (eg 'ghc', 'ghc-git' or 'msys2') \
                   \- case sensitive. (default: no filter)"
        )
