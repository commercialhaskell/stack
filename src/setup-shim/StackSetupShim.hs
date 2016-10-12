module StackSetupShim where
import Main
import Distribution.PackageDescription (PackageDescription, emptyHookedBuildInfo)
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
                { preRepl = \_ _ -> return emptyHookedBuildInfo
                , replHook = stackReplHook
                , postRepl = \_ _ _ _ -> return ()
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
