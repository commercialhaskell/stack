{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Docker.Handlers
License     : BSD-3-Clause

The module of this name differs as between Windows and non-Windows builds. This
is the Windows version.
-}

module Stack.Docker.Handlers
  ( handleSetGroups
  , handleSignals
  ) where

import           RIO.Process
                   ( ExitCodeException, proc , runProcess_, setDelegateCtlc )
import           Stack.Types.Config ( HasConfig )
import           Stack.Types.Docker ( DockerOpts (..))
import           Stack.Prelude
import           System.PosixCompat.Types ( GroupID )

handleSetGroups :: [GroupID] -> IO ()
handleSetGroups _ = pure ()

handleSignals ::
     (Exception e, HasConfig env)
  => DockerOpts
  -> Bool
  -> String
  -> RIO env (Either e ())
handleSignals docker keepStdinOpen containerID = do
  let args' = concat
        [ ["start"]
        , ["-a" | not docker.detach]
        , ["-i" | keepStdinOpen]
        , [containerID]
        ]
  finally
    (try $ proc "docker" args' $ runProcess_ . setDelegateCtlc False)
    ( unless (docker.persist || docker.detach) $
        readProcessNull "docker" ["rm", "-f", containerID]
          `catch` (\(_ :: ExitCodeException) -> pure ())
    )
