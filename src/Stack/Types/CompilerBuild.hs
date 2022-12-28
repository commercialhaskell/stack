{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.CompilerBuild
  ( CompilerBuild (..)
  , compilerBuildName
  , compilerBuildSuffix
  , parseCompilerBuild
  ) where

import           Stack.Prelude
import           Pantry.Internal.AesonExtended ( FromJSON, parseJSON, withText )
import           Data.Text as T

data CompilerBuild
    = CompilerBuildStandard
    | CompilerBuildSpecialized String
    deriving Show

instance FromJSON CompilerBuild where
    -- Strange structuring is to give consistent error messages
    parseJSON =
        withText
            "CompilerBuild"
            (either (fail . show) pure . parseCompilerBuild . T.unpack)

-- | Descriptive name for compiler build
compilerBuildName :: CompilerBuild -> String
compilerBuildName CompilerBuildStandard = "standard"
compilerBuildName (CompilerBuildSpecialized s) = s

-- | Suffix to use for filenames/directories constructed with compiler build
compilerBuildSuffix :: CompilerBuild -> String
compilerBuildSuffix CompilerBuildStandard = ""
compilerBuildSuffix (CompilerBuildSpecialized s) = '-' : s

-- | Parse compiler build from a String.
parseCompilerBuild :: (MonadThrow m) => String -> m CompilerBuild
parseCompilerBuild "" = pure CompilerBuildStandard
parseCompilerBuild "standard" = pure CompilerBuildStandard
parseCompilerBuild name = pure (CompilerBuildSpecialized name)
