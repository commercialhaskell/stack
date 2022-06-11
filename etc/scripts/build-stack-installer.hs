{- stack script
    --resolver nightly-2022-06-10
    --install-ghc
    --package nsis
-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String
import System.Environment

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

-- Note that it is *required* to use a NSIS compiler that supports long strings,
-- to avoid corrupting the user's $PATH.

main :: IO ()
main = do
  [srcPath, execPath, nsiPath, stackVersionStr] <- getArgs

  writeFile (fromString nsiPath) $ nsis $ do
    _ <- constantStr "Name" "Haskell Stack"

    name "$Name"
    outFile $ fromString execPath
    installDir "$APPDATA/local/bin"
    installDirRegKey HKCU "Software/$Name" "Install_Dir"
    requestExecutionLevel User

    page Directory
    page Components
    page InstFiles

    unpage Components
    unpage InstFiles

    section "Install Haskell Stack" [Required] $ do
      setOutPath "$INSTDIR"
      file [OName "stack.exe"] $ fromString srcPath

      -- Write the installation path into the registry
      writeRegStr HKCU "SOFTWARE/$Name" "Install_Dir" "$INSTDIR"

      -- Write the uninstall keys for Windows
      writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "DisplayName" "$Name"
      writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "DisplayVersion" (str stackVersionStr)
      writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "UninstallString" "\"$INSTDIR/uninstall-stack.exe\""
      writeRegDWORD HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "NoModify" 1
      writeRegDWORD HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "NoRepair" 1
      writeUninstaller "uninstall-stack.exe"

    section "Add to user %PATH%"
      [ Description "Add installation directory to user %PATH% to allow running Stack in the console."
      ] $ do
        setEnvVarPrepend HKCU "PATH" "$INSTDIR"

    section "Set %STACK_ROOT% to recommended default"
      [ Description "Set %STACK_ROOT% to C:\\sr to workaround issues with long paths."
      ] $ do
        setEnvVar HKCU "STACK_ROOT" "C:\\sr"

    -- Uninstallation sections. (Any section prepended with "un." is an
    -- uninstallation option.)
    section "un.stack" [] $ do
      deleteRegKey HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
      deleteRegKey HKCU "Software/$Name"

      delete [] "$INSTDIR/stack.exe"
      delete [] "$INSTDIR/uninstall-stack.exe"
      rmdir [] "$INSTDIR" -- will not remove if not empty

    -- The description text is not actually added to the uninstaller as of
    -- nsis-0.3
    section "un.Install location on %PATH%"
      [ Description "Remove $INSTDIR from the user %PATH%. There may be other programs installed in that location."
      ] $ do
        setEnvVarRemove HKCU "PATH" "$INSTDIR"

    section "un.Set %STACK_ROOT% to recommended default"
      [ Description "Remove setting of %STACK_ROOT% to C:\\sr."
      ] $ do
        deleteEnvVar HKCU "STACK_ROOT"

    section "un.Compilers installed by stack"
      [ Unselected
      , Description "Remove %LOCALAPPDATA%/Programs/stack, which contains compilers that have been installed by Stack."
      ] $ do
        rmdir [Recursive] "$LOCALAPPDATA/Programs/stack"

    section "un.stack snapshots and configuration"
      [ Unselected
      , Description "Remove %APPDATA%/stack, which contains the user-defined global stack.yaml and the snapshot/compilation cache."
      ] $ do
        rmdir [Recursive] "$APPDATA/stack"
