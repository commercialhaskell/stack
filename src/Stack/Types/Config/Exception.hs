{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Stack.Types.Config.Exception
  ( ConfigException (..)
  , ConfigPrettyException (..)
  , ParseAbsolutePathException (..)
  ) where

import qualified Data.Text as T
import           Data.Yaml ( ParseException )
import qualified Data.Yaml as Yaml
import           Distribution.System ( Arch )
import           Path( dirname, filename )
import           Stack.Prelude
import           Stack.Types.ConfigMonoid
                   ( configMonoidAllowDifferentUserName
                   , configMonoidGHCVariantName, configMonoidSystemGHCName
                   )
import           Stack.Types.MsysEnvironment ( MsysEnvironment )
import           Stack.Types.Version
                   ( VersionRange, stackVersion, versionRangeText )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Config" module.
data ConfigException
  = ParseCustomSnapshotException Text ParseException
  | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
  | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
  | UnableToExtractArchive Text (Path Abs File)
  | BadStackVersionException VersionRange
  | NoSuchDirectory FilePath
  | ParseGHCVariantException String
  | BadStackRoot (Path Abs Dir)
  | Won'tCreateStackRootInDirectoryOwnedByDifferentUser
      (Path Abs Dir)
      (Path Abs Dir)
    -- ^ @$STACK_ROOT@, parent dir
  | UserDoesn'tOwnDirectory (Path Abs Dir)
  | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
  | NixRequiresSystemGhc
  | NoSnapshotWhenUsingNoProject
  | NoLTSWithMajorVersion Int
  | NoLTSFound
  deriving (Show, Typeable)

instance Exception ConfigException where
  displayException (ParseCustomSnapshotException url exception) = concat
    [ "Error: [S-8981]\n"
    , "Could not parse '"
    , T.unpack url
    , "':\n"
    , Yaml.prettyPrintParseException exception
    , "\nSee https://docs.haskellstack.org/en/stable/custom_snapshot/"
    ]
  displayException (NoProjectConfigFound dir mcmd) = concat
    [ "Error: [S-2206]\n"
    , "Unable to find a stack.yaml file in the current directory ("
    , toFilePath dir
    , ") or its ancestors"
    , case mcmd of
        Nothing -> ""
        Just cmd -> "\nRecommended action: stack " ++ T.unpack cmd
    ]
  displayException (UnexpectedArchiveContents dirs files) = concat
    [ "Error: [S-4964]\n"
    , "When unpacking an archive specified in your stack.yaml file, "
    , "did not find expected contents. Expected: a single directory. Found: "
    , show ( map (toFilePath . dirname) dirs
           , map (toFilePath . filename) files
           )
    ]
  displayException (UnableToExtractArchive url file) = concat
    [ "Error: [S-2040]\n"
    , "Archive extraction failed. Tarballs and zip archives are supported, \
      \couldn't handle the following URL, "
    , T.unpack url
    , " downloaded to the file "
    , toFilePath $ filename file
    ]
  displayException (BadStackVersionException requiredRange) = concat
    [ "Error: [S-1641]\n"
    , "The version of Stack you are using ("
    , show stackVersion
    , ") is outside the required\n"
    ,"version range specified in stack.yaml ("
    , T.unpack (versionRangeText requiredRange)
    , ").\n"
    , "You can upgrade Stack by running:\n\n"
    , "stack upgrade"
    ]
  displayException (NoSuchDirectory dir) = concat
    [ "Error: [S-8773]\n"
    , "No directory could be located matching the supplied path: "
    , dir
    ]
  displayException (ParseGHCVariantException v) = concat
    [ "Error: [S-3938]\n"
    , "Invalid ghc-variant value: "
    , v
    ]
  displayException (BadStackRoot stackRoot) = concat
    [ "Error: [S-8530]\n"
    , "Invalid Stack root: '"
    , toFilePath stackRoot
    , "'. Please provide a valid absolute path."
    ]
  displayException (Won'tCreateStackRootInDirectoryOwnedByDifferentUser envStackRoot parentDir) = concat
    [ "Error: [S-7613]\n"
    , "Preventing creation of Stack root '"
    , toFilePath envStackRoot
    , "'. Parent directory '"
    , toFilePath parentDir
    , "' is owned by someone else."
    ]
  displayException (UserDoesn'tOwnDirectory dir) = concat
    [ "Error: [S-8707]\n"
    , "You are not the owner of '"
    , toFilePath dir
    , "'. Aborting to protect file permissions."
    , "\nRetry with '--"
    , T.unpack configMonoidAllowDifferentUserName
    , "' to disable this precaution."
    ]
  displayException ManualGHCVariantSettingsAreIncompatibleWithSystemGHC = T.unpack $ T.concat
    [ "Error: [S-3605]\n"
    , "Stack can only control the "
    , configMonoidGHCVariantName
    , " of its own GHC installations. Please use '--no-"
    , configMonoidSystemGHCName
    , "'."
    ]
  displayException NixRequiresSystemGhc = T.unpack $ T.concat
    [ "Error: [S-6816]\n"
    , "Stack's Nix integration is incompatible with '--no-system-ghc'. "
    , "Please use '--"
    , configMonoidSystemGHCName
    , "' or disable the Nix integration."
    ]
  displayException NoSnapshotWhenUsingNoProject =
    "Error: [S-5027]\n"
    ++ "When using the script command, you must provide a snapshot argument"
  displayException (NoLTSWithMajorVersion n) = concat
    [ "Error: [S-3803]\n"
    , "No LTS release found with major version "
    , show n
    , "."
    ]
  displayException NoLTSFound =
    "Error: [S-5472]\n"
    ++ "No LTS releases found."

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Config" module.
data ConfigPrettyException
  = ParseConfigFileException !(Path Abs File) !ParseException
  | StackWorkEnvNotRelativeDir !String
  | DuplicateLocalPackageNames ![(PackageName, [PackageLocation])]
  | BadMsysEnvironment !MsysEnvironment !Arch
  | NoMsysEnvironmentBug
  | ConfigFileNotProjectLevelBug
  deriving (Show, Typeable)

instance Pretty ConfigPrettyException where
  pretty (ParseConfigFileException configFile exception) =
    "[S-6602]"
    <> line
    <> fillSep
         [ flow "Stack could not load and parse"
         , pretty configFile
         , flow "as a YAML configuraton file."
         ]
    <> blankLine
    <> flow "While loading and parsing, Stack encountered the following \
            \error:"
    <> blankLine
    <> string (Yaml.prettyPrintParseException exception)
    <> blankLine
    <> fillSep
         [ flow "For help about the content of Stack's YAML configuration \
                \files, see (for the most recent release of Stack)"
         ,    style
                Url
                "http://docs.haskellstack.org/en/stable/yaml_configuration/"
           <> "."
         ]
  pretty (StackWorkEnvNotRelativeDir x) =
    "[S-7462]"
    <> line
    <> flow "Stack failed to interpret the value of the STACK_WORK \
            \environment variable as a valid relative path to a directory. \
            \Stack will not accept an absolute path. A path containing a \
            \.. (parent directory) component is not valid."
    <> blankLine
    <> fillSep
         [ flow "If set, Stack expects the value to identify the location \
                \of Stack's work directory, relative to the root directory \
                \of the project or package. Stack encountered the value:"
         , style Error (fromString x) <> "."
         ]
  pretty (DuplicateLocalPackageNames pairs) =
    "[S-5470]"
    <> line
    <> fillSep
         [ flow "The same package name is used in more than one project \
                \package or"
         , style Shell "extra-deps" <> "."
         ]
    <> mconcat (map go pairs)
   where
    go (name, dirs) =
         blankLine
      <> fillSep
           [ style Error (fromPackageName name)
           , flow "used in:"
           ]
      <> line
      <> bulletedList (map (fromString . T.unpack . textDisplay) dirs)
  pretty (BadMsysEnvironment msysEnv arch) =
    "[S-6854]"
    <> line
    <> fillSep
         [ flow "The specified MSYS2 environment"
         , style Error (fromString $ show msysEnv)
         , flow "is not consistent with the architecture"
         , fromString (show arch) <> "."
         ]
  pretty NoMsysEnvironmentBug = bugPrettyReport "[S-5006]" $
    flow "No default MSYS2 environment."
  pretty ConfigFileNotProjectLevelBug = bugPrettyReport "[S-8398]" $
    flow "The configuration file is not a project-level one."

instance Exception ConfigPrettyException

data ParseAbsolutePathException
  = ParseAbsolutePathException String String
  deriving (Show, Typeable)

instance Exception ParseAbsolutePathException where
  displayException (ParseAbsolutePathException envVar dir) = concat
    [ "Error: [S-9437]\n"
    , "Failed to parse "
    , envVar
    , " environment variable (expected absolute directory): "
    , dir
    ]
