{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef USE_GIT_INFO
{-# LANGUAGE TemplateHaskell   #-}
#endif

-- Extracted from Main so that the Main module does not use CPP or TH,
-- and therefore doesn't need to be recompiled as often.
module BuildInfo
  ( versionString'
  , hpackVersion
  ) where

#ifndef HIDE_DEP_VERSIONS
import qualified Build_stack
#endif
#ifdef USE_GIT_INFO
import           Data.Version ( versionBranch )
#else
import           Data.Version ( showVersion, versionBranch )
#endif
import           Distribution.System ( buildArch )
import qualified Distribution.Text as Cabal ( display )
#ifdef USE_GIT_INFO
import           GitHash ( giCommitCount, tGitInfoCwdTry )
#endif
#ifdef USE_GIT_INFO
import           Options.Applicative.Simple ( simpleVersion )
#endif
import           Stack.Prelude
import qualified Paths_stack as Meta

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

-- | Hpack version we're compiled against
hpackVersion :: String
hpackVersion = VERSION_hpack
