{-# LANGUAGE CPP            #-}
{-# LANGUAGE PackageImports #-}
module StackSetupShim where
import Main
#if defined(MIN_VERSION_Cabal)
#if MIN_VERSION_Cabal(3,8,1)
import Distribution.PackageDescription (PackageDescription, emptyHookedBuildInfo)
#else
import "Cabal" Distribution.PackageDescription (PackageDescription, emptyHookedBuildInfo)
#endif
#else
import Distribution.PackageDescription (PackageDescription, emptyHookedBuildInfo)
#endif
import Distribution.Simple
import Distribution.Simple.Build
import Distribution.Simple.Setup (ReplFlags, fromFlag, replDistPref, replVerbosity)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import System.Environment (getArgs)

mainOverride :: IO ()
mainOverride = do
    args <- getArgs
    if "repl" `elem` args && "stack-initial-build-steps" `elem` args
        then do
            defaultMainWithHooks simpleUserHooks
                { preRepl = \_ _ -> pure emptyHookedBuildInfo
                , replHook = stackReplHook
                , postRepl = \_ _ _ _ -> pure ()
                }
        else main

stackReplHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO ()
stackReplHook pkg_descr lbi hooks flags args = do
    let distPref = fromFlag (replDistPref flags)
        verbosity = fromFlag (replVerbosity flags)
    case args of
        ("stack-initial-build-steps":rest)
            | null rest -> initialBuildSteps distPref pkg_descr lbi verbosity
            | otherwise ->
                fail "Misuse of running Setup.hs with stack-initial-build-steps, expected no arguments"
        _ -> replHook simpleUserHooks pkg_descr lbi hooks flags args
