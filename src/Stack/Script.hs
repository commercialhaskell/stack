{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.Script
    ( scriptCmd
    ) where

import           Control.Exception          (assert)
import           Control.Monad              (unless, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger
import qualified Data.ByteString.Char8      as S8
import qualified Data.Conduit.List          as CL
import           Data.List.Split            (splitWhen)
import           Data.Monoid
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Path
import           Path.IO
import qualified Stack.Build
import           Stack.Exec
import           Stack.GhcPkg               (ghcPkgExeName)
import           Stack.Options.ScriptParser
import           Stack.Runners
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.PackageName
import           System.FilePath            (dropExtension, replaceExtension)
import           System.Process.Read

-- | Run a Stack Script
scriptCmd :: ScriptOpts -> GlobalOpts -> IO ()
scriptCmd opts go' = do
    print opts
    let go = go'
            { globalConfigMonoid = (globalConfigMonoid go')
                { configMonoidInstallGHC = First $ Just True
                }
            , globalStackYaml = SYLNoConfig
            }
    withBuildConfigAndLock go $ \lk -> do
        -- Some warnings in case the user somehow tries to set a
        -- stack.yaml location
        case globalStackYaml go' of
          SYLOverride fp -> $logWarn $ T.pack
            $ "Ignoring override stack.yaml file for script command: " ++ fp
          SYLDefault -> return ()
          SYLNoConfig -> assert False (return ())

        config <- view configL
        menv <- liftIO $ configEnvOverride config defaultEnvSettings
        wc <- view $ actualCompilerVersionL.whichCompilerL

        let targets = concatMap wordsComma $ soPackages opts
            targetsSet = Set.fromList targets

        -- Ensure only package names are provided. We do not allow
        -- overriding packages in a snapshot.
        mapM_ parsePackageNameFromString targets

        unless (null targets) $ do
            -- Optimization: use the relatively cheap ghc-pkg list
            -- --simple-output to check which packages are installed
            -- already. If all needed packages are available, we can
            -- skip the (rather expensive) build call below.
            bss <- sinkProcessStdout
                Nothing menv (ghcPkgExeName wc)
                ["list", "--simple-output"] CL.consume -- FIXME use the package info from envConfigPackages, or is that crazy?
            let installed = Set.fromList
                          $ map toPackageName
                          $ words
                          $ S8.unpack
                          $ S8.concat bss
            if Set.null $ Set.difference targetsSet installed
                then $logDebug "All packages already installed"
                else do
                    $logDebug "Missing packages, performing installation"
                    Stack.Build.build (const $ return ()) lk defaultBuildOptsCLI
                        { boptsCLITargets = map T.pack targets
                        }

        let ghcArgs = concat
                [ ["-hide-all-packages"]
                , map (\x -> "-package" ++ x)
                    $ Set.toList
                    $ Set.insert "base" targetsSet
                , case soCompile opts of
                    SEInterpret -> []
                    SECompile -> []
                    SEOptimize -> ["-O2"]
                ]
        munlockFile lk -- Unlock before transferring control away.
        case soCompile opts of
          SEInterpret -> exec menv ("run" ++ compilerExeName wc)
                (ghcArgs ++ soFile opts : soArgs opts)
          _ -> do
            file <- resolveFile' $ soFile opts
            let dir = parent file
            void $ readProcessStderrStdout
              (Just dir)
              menv
              (compilerExeName wc)
              (ghcArgs ++ [soFile opts])
            exec menv (toExeName $ toFilePath file) (soArgs opts)
  where
    toPackageName = reverse . drop 1 . dropWhile (/= '-') . reverse

    -- Like words, but splits on both commas and spaces
    wordsComma = splitWhen (\c -> c == ' ' || c == ',')

    toExeName fp =
      if isWindows
        then replaceExtension fp "exe"
        else dropExtension fp

#ifdef WINDOWS
    isWindows = True
#else
    isWindows = False
#endif
