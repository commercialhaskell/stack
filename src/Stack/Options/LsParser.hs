{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions to parse command line arguments for Stack's @ls@ command.
module Stack.Options.LsParser
  ( lsOptsParser
  ) where

import qualified Options.Applicative as OA
import           Options.Applicative ( idm )
import           Options.Applicative.Builder.Extra ( boolFlags )
import           Stack.Constants ( globalFooter )
import           Stack.Ls
                   ( ListStylesOpts (..), ListToolsOpts (..), LsCmdOpts (..)
                   , LsCmds (..), LsView (..), SnapshotOpts (..)
                   )
import           Stack.Options.DotParser ( listDepsOptsParser )
import           Stack.Prelude

-- | Parse command line arguments for Stack's @ls@ command.
lsOptsParser :: OA.Parser LsCmdOpts
lsOptsParser = LsCmdOpts
  <$> OA.hsubparser (lsSnapCmd <> lsDepsCmd <> lsStylesCmd <> lsToolsCmd)

lsCmdOptsParser :: OA.Parser LsCmds
lsCmdOptsParser = LsSnapshot <$> lsViewSnapCmd

lsDepOptsParser :: OA.Parser LsCmds
lsDepOptsParser = LsDependencies <$> listDepsOptsParser

lsStylesOptsParser :: OA.Parser LsCmds
lsStylesOptsParser = LsStyles <$> listStylesOptsParser

lsToolsOptsParser :: OA.Parser LsCmds
lsToolsOptsParser = LsTools <$> listToolsOptsParser

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

lsSnapCmd :: OA.Mod OA.CommandFields LsCmds
lsSnapCmd = OA.command "snapshots" $
  OA.info lsCmdOptsParser $
       OA.progDesc "View snapshots. (default: local)"
    <> OA.footer localSnapshotMsg

lsDepsCmd :: OA.Mod OA.CommandFields LsCmds
lsDepsCmd = OA.command "dependencies" $
  OA.info lsDepOptsParser $
       OA.progDesc "View the dependencies."
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

lsViewLocalCmd :: OA.Mod OA.CommandFields LsView
lsViewLocalCmd = OA.command "local" $
  OA.info (pure Local) $
       OA.progDesc "View local snapshots."
    <> OA.footer localSnapshotMsg

lsViewRemoteCmd :: OA.Mod OA.CommandFields LsView
lsViewRemoteCmd = OA.command "remote" $
  OA.info (pure Remote) $
       OA.progDesc "View remote snapshots."
    <> OA.footer pagerMsg

pagerMsg :: String
pagerMsg =
  "On a terminal, uses a pager, if one is available. Respects the PAGER \
  \environment variable (subject to that, prefers pager 'less' to 'more')."

localSnapshotMsg :: String
localSnapshotMsg =
  "A local snapshot is identified by a hash code. " <> pagerMsg
