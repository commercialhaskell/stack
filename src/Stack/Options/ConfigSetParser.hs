{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.ConfigSetParser
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @config set@ command.
-}

module Stack.Options.ConfigSetParser
  ( configCmdSetParser
  ) where

import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Stack.Prelude
import           Stack.Types.ConfigMonoid
                   ( configMonoidInstallGHCName
                   , configMonoidInstallMsysName
                   , configMonoidRecommendStackUpgradeName
                   , configMonoidSystemGHCName
                   )
import           Stack.Types.ConfigSetOpts
                   ( CommandScope (..), ConfigCmdSet (..) )
import           Stack.Types.Snapshot ( readAbstractSnapshot )

-- | Parse command line arguments for Stack's @config set@ command.
configCmdSetParser :: OA.Parser ConfigCmdSet
configCmdSetParser =
  OA.hsubparser $
    mconcat
      [ OA.command "snapshot"
          ( OA.info
              (   ConfigCmdSetSnapshot
              <$> OA.argument
                    readAbstractSnapshot
                    (  OA.metavar "SNAPSHOT"
                    <> OA.help "E.g. \"nightly\" or \"lts-24.18\"" ))
              ( OA.progDesc
                  "Change the snapshot of the current project." ))
      , OA.command "resolver"
          ( OA.info
              (   ConfigCmdSetResolver
              <$> OA.argument
                    readAbstractSnapshot
                    (  OA.metavar "SNAPSHOT"
                    <> OA.help "E.g. \"nightly\" or \"lts-24.18\"" ))
              ( OA.progDesc
                  "Change the snapshot of the current project, using the \
                  \resolver key." ))
      , OA.command (T.unpack configMonoidSystemGHCName)
          ( OA.info
              (   ConfigCmdSetSystemGhc
              <$> globalScopeFlag
              <*> boolArgument )
              ( OA.progDesc
                  "Configure whether or not Stack should use a system GHC \
                  \installation." ))
      , OA.command (T.unpack configMonoidInstallGHCName)
          ( OA.info
              (   ConfigCmdSetInstallGhc
              <$> globalScopeFlag
              <*> boolArgument )
              ( OA.progDesc
                  "Configure whether or not Stack should automatically install \
                  \GHC when necessary." ))
      , OA.command (T.unpack configMonoidInstallMsysName)
          ( OA.info
              (   ConfigCmdSetInstallMsys
              <$> globalScopeFlag
              <*> boolArgument )
              ( OA.progDesc
                  "Configure whether or not Stack should automatically install \
                  \MSYS2 when necessary." ))
      , OA.command (T.unpack configMonoidRecommendStackUpgradeName)
          ( OA.info
              (   ConfigCmdSetRecommendStackUpgrade
              <$> projectScopeFlag
              <*> boolArgument )
              ( OA.progDesc
                  "Configure whether or not Stack should notify the user if it \
                  \identifes a new version of Stack is available." ))
      , OA.command "package-index"
          ( OA.info
              ( OA.hsubparser $
                  OA.command "download-prefix"
                    ( OA.info
                        (   ConfigCmdSetDownloadPrefix
                        <$> globalScopeFlag
                        <*> urlArgument )
                        ( OA.progDesc
                            "Configure download prefix for Stack's package \
                            \index." )))
              ( OA.progDesc
                  "Configure Stack's package index" ))
      ]

globalScopeFlag :: OA.Parser CommandScope
globalScopeFlag = OA.flag
  CommandScopeProject
  CommandScopeGlobal
  (  OA.long "global"
  <> OA.help
       "Modify the user-specific global configuration file ('config.yaml') \
       \instead of the project-level configuration file ('stack.yaml')."
  )

projectScopeFlag :: OA.Parser CommandScope
projectScopeFlag = OA.flag
  CommandScopeGlobal
  CommandScopeProject
  (  OA.long "project"
  <> OA.help
       "Modify the project-level configuration file ('stack.yaml') instead of \
       \the user-specific global configuration file ('config.yaml')."
  )

boolArgument :: OA.Parser Bool
boolArgument = OA.argument
  readBool
  (  OA.metavar "true|false"
  <> OA.completeWith ["true", "false"]
  )

readBool :: OA.ReadM Bool
readBool = do
  s <- OA.readerAsk
  case s of
    "true" -> pure True
    "false" -> pure False
    _ -> OA.readerError ("Invalid value " ++ show s ++
           ": Expected \"true\" or \"false\"")

urlArgument :: OA.Parser Text
urlArgument = OA.strArgument
  (  OA.metavar "URL"
  <> OA.value defaultDownloadPrefix
  <> OA.showDefault
  <> OA.help
       "Location of package index. It is highly recommended to use only the \
       \official Hackage server or a mirror."
  )
