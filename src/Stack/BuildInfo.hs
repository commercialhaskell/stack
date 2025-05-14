{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef USE_GIT_INFO
{-# LANGUAGE TemplateHaskell   #-}
#endif

{-|
Module      : Stack.BuildInfo
License     : BSD-3-Clause

Extracted from "Stack" so that module does not use CPP or Template Haskell, and
therefore doesn't need to be recompiled as often.
-}

module Stack.BuildInfo
  ( versionString'
  , hpackVersion
  , maybeGitHash
  ) where

#ifndef HIDE_DEP_VERSIONS
import qualified Build_stack
#endif
import           Data.Version ( versionBranch )
import           Distribution.System ( buildArch )
import qualified Distribution.Text as Cabal ( display )
#ifdef USE_GIT_INFO
import           GitHash ( giCommitCount, giHash, tGitInfoCwdTry )
import           Options.Applicative.Simple ( simpleVersion )
#endif
import qualified Paths_stack as Meta
import           Stack.Constants ( isStackUploadDisabled )
import           Stack.Prelude
#ifndef USE_GIT_INFO
import           Stack.Types.Version ( showStackVersion )
#endif

-- | The output of @stack --version@.
versionString' :: String
#ifdef USE_GIT_INFO
versionString' = concat $ concat
  [ [$(simpleVersion Meta.version)]
    -- Leave out number of commits for --depth=1 clone
    -- See https://github.com/commercialhaskell/stack/issues/792
  , case giCommitCount <$> $$tGitInfoCwdTry of
      Left _ -> []
      Right 1 -> []
      Right count -> [" (", show count, " commits)"]
  , [afterVersion]
  ]
#else
versionString' = showStackVersion ++ afterVersion
#endif
 where
  afterVersion = concat
    [ preReleaseString
    , ' ' : Cabal.display buildArch
    , depsString
    , warningString
    , stackUploadDisabledWarningString
    ]
  preReleaseString =
    case versionBranch Meta.version of
      (_:y:_) | even y -> " PRE-RELEASE"
      (_:_:z:_) | even z -> " RELEASE-CANDIDATE"
      _ -> ""
#ifdef HIDE_DEP_VERSIONS
  depsString = " hpack-" ++ VERSION_hpack
#else
  depsString = "\nCompiled with:\n" ++ unlines (map ("- " ++) Build_stack.deps)
#endif
#ifdef SUPPORTED_BUILD
  warningString = ""
#else
  warningString = unlines
    [ ""
    , "Warning: this is an unsupported build that may use different versions of"
    , "dependencies and GHC than the officially released binaries, and therefore may"
    , "not behave identically.  If you encounter problems, please try the latest"
    , "official build by running 'stack upgrade --force-download'."
    ]
#endif
  stackUploadDisabledWarningString = if isStackUploadDisabled
    then unlines
      [ ""
      , "Warning: 'stack upload' is disabled and will not make HTTP request(s). It will"
      , "output information about the HTTP request(s) that would have been made if it"
      , "was enabled."
      ]
    else ""

-- | Hpack version we're compiled against
hpackVersion :: String
hpackVersion = VERSION_hpack

-- | If USE_GIT_INFO is enabled, the Git hash in the build directory, otherwise
-- Nothing.
maybeGitHash :: Maybe String
maybeGitHash =
#ifdef USE_GIT_INFO
  (either (const Nothing) (Just . giHash) $$tGitInfoCwdTry)
#else
  Nothing
#endif
