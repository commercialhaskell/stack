{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Docker.Handlers
License     : BSD-3-Clause

The module of this name differs as between Windows and non-Windows builds. This
is the non-Windows version.
-}

module Stack.Docker.Handlers
  ( handleSetGroups
  , handleSignals
  ) where

import           RIO.Process
                   ( ExitCodeException, proc, runProcess_, setDelegateCtlc )
import           Stack.Prelude
import           Stack.Types.Config ( HasConfig )
import           Stack.Types.Docker ( DockerOpts (..) )
import           System.Posix.Signals
                   ( Handler (..), installHandler, sigABRT, sigHUP, sigINT
                   , sigPIPE, sigTERM, sigUSR1, sigUSR2
                   )
import qualified System.Posix.User as PosixUser
import           System.PosixCompat.Types ( GroupID )

handleSetGroups :: [GroupID] -> IO ()
handleSetGroups = PosixUser.setGroups

-- MSS 2018-08-30 can the CPP below be removed entirely, and instead exec the
-- `docker` process so that it can handle the signals directly?
handleSignals ::
     (Exception e, HasConfig env)
  => DockerOpts
  -> Bool
  -> String
  -> RIO env (Either e ())
handleSignals docker keepStdinOpen containerID = do
  run <- askRunInIO
  oldHandlers <- forM signals $ \sig -> do
    let sigHandler = run $ do
          readProcessNull
            "docker"
            ["kill", "--signal=" ++ show sig, containerID]
          when (sig `elem` [sigTERM, sigABRT]) $ do
            -- Give the container 30 seconds to exit gracefully, then send a
            -- sigKILL to force it
            threadDelay 30000000
            readProcessNull "docker" ["kill", containerID]
    oldHandler <- liftIO $ installHandler sig (Catch sigHandler) Nothing
    pure (sig, oldHandler)
  let args' = concat
        [ ["start"]
        , ["-a" | not docker.detach]
        , ["-i" | keepStdinOpen]
        , [containerID]
        ]
  finally
    (try $ proc "docker" args' $ runProcess_ . setDelegateCtlc False)
    ( do unless (docker.persist || docker.detach) $
           readProcessNull "docker" ["rm", "-f", containerID]
             `catch` (\(_ :: ExitCodeException) -> pure ())
         forM_ oldHandlers $ \(sig, oldHandler) ->
           liftIO $ installHandler sig oldHandler Nothing
    )
 where
  signals = [sigINT, sigABRT, sigHUP, sigPIPE, sigTERM, sigUSR1, sigUSR2]
