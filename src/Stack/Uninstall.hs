{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Function related to Stack's @uninstall@ command.
module Stack.Uninstall
  ( uninstallCmd
  ) where

import          Stack.Constants ( osIsWindows )
import          Stack.Prelude
import          Stack.Runners ( ShouldReexec (..), withConfig )
import          Stack.Types.Config
                  ( configL, configLocalBin, configLocalProgramsBase
                  , stackGlobalConfigL, stackRootL
                  )
import          Stack.Types.Runner ( Runner )

-- | Function underlying the @stack uninstall@ command. Display help for the
-- command.
uninstallCmd :: () -> RIO Runner ()
uninstallCmd () = withConfig NoReexec $ do
  stackRoot <- view stackRootL
  globalConfig <- view stackGlobalConfigL
  programsDir <- view $ configL.to configLocalProgramsBase
  localBinDir <- view $ configL.to configLocalBin
  let toStyleDoc = style Dir . fromString . toFilePath
      stackRoot' = toStyleDoc stackRoot
      globalConfig' = toStyleDoc globalConfig
      programsDir' = toStyleDoc programsDir
      localBinDir' = toStyleDoc localBinDir
  prettyInfo $
       vsep
         [ flow "To uninstall Stack, it should be sufficient to delete:"
         , hang 4 $ fillSep
             [ flow "(1) the directory containing Stack's tools"
             , "(" <> softbreak <> programsDir' <> softbreak <> ");"
             ]
         , hang 4 $ fillSep
             [ flow "(2) the Stack root directory"
             , "(" <> softbreak <> stackRoot' <> softbreak <> ");"
             ]
         , hang 4 $ fillSep
             [ flow "(3) if different, the directory containing "
             , flow "Stack's global YAML configuration file"
             , parens globalConfig' <> ";"
             , "and"
             ]
         , hang 4 $ fillSep
             [ flow "(4) the 'stack' executable file (see the output"
             , flow "of command"
             , howToFindStack <> ","
             , flow "if Stack is on the PATH;"
             , flow "Stack is often installed in"
             , localBinDir' <> softbreak <> ")."
             ]
         , fillSep
             [flow "You may also want to delete"
             , style File ".stack-work"
             , flow "directories in any Haskell projects that you have built."
             ]
         ]
      <> blankLine
      <> vsep
           [ fillSep
               [ flow "To uninstall completely a Stack-supplied tool (such as \
                      \GHC or, on Windows, MSYS2), delete from Stack's tools \
                      \directory"
               , parens programsDir' <> ":"
               ]
           , hang 4 $ fillSep
               [ flow "(1) the tool's subdirectory;"
               ]
           , hang 4 $ fillSep
               [ flow "(2) the tool's archive file"
               , parens (style File "<tool>.tar.xz") <> "; and"
               ]
           , hang 4 $ fillSep
               [ flow "(3) the file marking that the tool is installed"
               , parens (style File "<tool>.installed") <> "."
               ]
           ]
 where
  styleShell = style Shell
  howToFindStack
    | osIsWindows = styleShell "where.exe stack"
    | otherwise   = styleShell "which stack"
