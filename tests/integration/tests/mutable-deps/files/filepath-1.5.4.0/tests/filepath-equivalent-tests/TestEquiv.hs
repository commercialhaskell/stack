{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck hiding ((==>))
import TestUtil
import Prelude as P
import Data.Char (isAsciiLower, isAsciiUpper)
import Gen

import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P
import qualified Legacy.System.FilePath.Windows as LW
import qualified Legacy.System.FilePath.Posix as LP


main :: IO ()
main = defaultMain equivalentTests


equivalentTests :: TestTree
equivalentTests = testGroup "equivalence"
  [ testProperties "windows"
    [
      ( "pathSeparator"
      , property $ W.pathSeparator == LW.pathSeparator
      )
      ,
      ( "pathSeparators"
      , property $ W.pathSeparators == LW.pathSeparators
      )
      ,
      ( "isPathSeparator"
      , property $ \p -> W.isPathSeparator p == LW.isPathSeparator p
      )
      ,
      ( "searchPathSeparator"
      , property $ W.searchPathSeparator == LW.searchPathSeparator
      )
      ,
      ( "isSearchPathSeparator"
      , property $ \p -> W.isSearchPathSeparator p == LW.isSearchPathSeparator p
      )
      ,
      ( "extSeparator"
      , property $ W.extSeparator == LW.extSeparator
      )
      ,
      ( "isExtSeparator"
      , property $ \p -> W.isExtSeparator p == LW.isExtSeparator p
      )
      ,
      ( "splitSearchPath"
      , property $ \(xs :: WindowsFilePaths)
        -> let p = (intercalate ";" (altShow <$> unWindowsFilePaths xs))
           in W.splitSearchPath p == LW.splitSearchPath p
      )
      ,
      ( "splitExtension"
      , property $ \(altShow @WindowsFilePath -> p) -> W.splitExtension p == LW.splitExtension p
      )
      ,
      ( "takeExtension"
      , property $ \(altShow @WindowsFilePath -> p) -> W.takeExtension p == LW.takeExtension p
      )
      ,
      ( "replaceExtension"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceExtension p s == LW.replaceExtension p s
      )
      ,
      ( "dropExtension"
      , property $ \(altShow @WindowsFilePath -> p) -> W.dropExtension p == LW.dropExtension p
      )
      ,
      ( "addExtension"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.addExtension p s == LW.addExtension p s
      )
      ,
      ( "hasExtension"
      , property $ \(altShow @WindowsFilePath -> p) -> W.hasExtension p == LW.hasExtension p
      )
      ,
      ( "splitExtensions"
      , property $ \(altShow @WindowsFilePath -> p) -> W.splitExtensions p == LW.splitExtensions p
      )
      ,
      ( "dropExtensions"
      , property $ \(altShow @WindowsFilePath -> p) -> W.dropExtensions p == LW.dropExtensions p
      )
      ,
      ( "takeExtensions"
      , property $ \p -> W.takeExtensions p == LW.takeExtensions p
      )
      ,
      ( "replaceExtensions"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceExtensions p s == LW.replaceExtensions p s
      )
      ,
      ( "isExtensionOf"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.isExtensionOf p s == LW.isExtensionOf p s
      )
      ,
      ( "stripExtension"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.stripExtension p s == LW.stripExtension p s
      )
      ,
      ( "splitFileName"
      , property $ \(altShow @WindowsFilePath -> p) -> W.splitFileName p == LW.splitFileName p
      )
      ,
      ( "takeFileName"
      , property $ \(altShow @WindowsFilePath -> p) -> W.takeFileName p == LW.takeFileName p
      )
      ,
      ( "replaceFileName"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceFileName p s == LW.replaceFileName p s
      )
      ,
      ( "dropFileName"
      , property $ \(altShow @WindowsFilePath -> p) -> W.dropFileName p == LW.dropFileName p
      )
      ,
      ( "takeBaseName"
      , property $ \(altShow @WindowsFilePath -> p) -> W.takeBaseName p == LW.takeBaseName p
      )
      ,
      ( "replaceBaseName"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceBaseName p s == LW.replaceBaseName p s
      )
      ,
      ( "takeDirectory"
      , property $ \(altShow @WindowsFilePath -> p) -> W.takeDirectory p == LW.takeDirectory p
      )
      ,
      ( "replaceDirectory"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceDirectory p s == LW.replaceDirectory p s
      )
      ,
      ( "combine"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.combine p s == LW.combine p s
      )
      ,
      ( "splitPath"
      , property $ \(altShow @WindowsFilePath -> p) -> W.splitPath p == LW.splitPath p
      )
      ,
      ( "joinPath"
      , property $ \(xs :: WindowsFilePaths) ->
         let p = altShow <$> unWindowsFilePaths xs
         in W.joinPath p == LW.joinPath p
      )
      ,
      ( "splitDirectories"
      , property $ \(altShow @WindowsFilePath -> p) -> W.splitDirectories p == LW.splitDirectories p
      )
      ,
      ( "splitDrive"
      , property $ \(altShow @WindowsFilePath -> p) -> W.splitDrive p == LW.splitDrive p
      )
      ,
      ( "joinDrive"
      , property $ \(altShow @WindowsFilePath -> p) s -> W.joinDrive p s == LW.joinDrive p s
      )
      ,
      ( "takeDrive"
      , property $ \(altShow @WindowsFilePath -> p) -> W.takeDrive p == LW.takeDrive p
      )
      ,
      ( "hasDrive"
      , property $ \(altShow @WindowsFilePath -> p) -> W.hasDrive p == LW.hasDrive p
      )
      ,
      ( "dropDrive"
      , property $ \(altShow @WindowsFilePath -> p) -> W.dropDrive p == LW.dropDrive p
      )
      ,
      ( "isDrive"
      , property $ \(altShow @WindowsFilePath -> p) -> W.isDrive p == LW.isDrive p
      )
      ,
      ( "hasTrailingPathSeparator"
      , property $ \(altShow @WindowsFilePath -> p) -> W.hasTrailingPathSeparator p == LW.hasTrailingPathSeparator p
      )
      ,
      ( "addTrailingPathSeparator"
      , property $ \(altShow @WindowsFilePath -> p) -> W.addTrailingPathSeparator p == LW.addTrailingPathSeparator p
      )
      ,
      ( "dropTrailingPathSeparator"
      , property $ \(altShow @WindowsFilePath -> p) -> W.dropTrailingPathSeparator p == LW.dropTrailingPathSeparator p
      )
      ,
      ( "normalise"
      , property $ \(altShow @WindowsFilePath -> p) -> case p of
                           (l:':':rs)
                             -- new filepath normalises "a:////////" to "A:\\"
                             -- see https://github.com/haskell/filepath/commit/cb4890aa03a5ee61f16f7a08dd2d964fffffb385
                             | isAsciiLower l || isAsciiUpper l
                             , let (seps, path) = span LW.isPathSeparator rs
                             , length seps > 1 -> let np = l : ':' : LW.pathSeparator : path in W.normalise np == LW.normalise np
                           _ -> W.normalise p == LW.normalise p
      )
      ,
      ( "equalFilePath"
      , property $ \p s -> W.equalFilePath p s == LW.equalFilePath p s
      )
      ,
      ( "makeRelative"
      , property $ \p s -> W.makeRelative p s == LW.makeRelative p s
      )
      ,
      ( "isRelative"
      , property $ \p -> W.isRelative p == LW.isRelative p
      )
      ,
      ( "isAbsolute"
      , property $ \p -> W.isAbsolute p == LW.isAbsolute p
      )
      ,
      ( "isValid"
      , property $ \p -> W.isValid p == LW.isValid p
      )
      ,
      ( "makeValid"
      , property $ \p -> W.makeValid p == LW.makeValid p
      )
    ],
    testProperties "posix" $ [
      ( "pathSeparator"
      , property $ P.pathSeparator == LP.pathSeparator
      )
      ,
      ( "pathSeparators"
      , property $ P.pathSeparators == LP.pathSeparators
      )
      ,
      ( "isPathSeparator"
      , property $ \p -> P.isPathSeparator p == LP.isPathSeparator p
      )
      ,
      ( "searchPathSeparator"
      , property $ P.searchPathSeparator == LP.searchPathSeparator
      )
      ,
      ( "isSearchPathSeparator"
      , property $ \p -> P.isSearchPathSeparator p == LP.isSearchPathSeparator p
      )
      ,
      ( "extSeparator"
      , property $ P.extSeparator == LP.extSeparator
      )
      ,
      ( "isExtSeparator"
      , property $ \p -> P.isExtSeparator p == LP.isExtSeparator p
      )
      ,
      ( "splitSearchPath"
      , property $ \p -> P.splitSearchPath p == LP.splitSearchPath p
      )
      ,
      ( "splitExtension"
      , property $ \p -> P.splitExtension p == LP.splitExtension p
      )
      ,
      ( "takeExtension"
      , property $ \p -> P.takeExtension p == LP.takeExtension p
      )
      ,
      ( "replaceExtension"
      , property $ \p s -> P.replaceExtension p s == LP.replaceExtension p s
      )
      ,
      ( "dropExtension"
      , property $ \p -> P.dropExtension p == LP.dropExtension p
      )
      ,
      ( "addExtension"
      , property $ \p s -> P.addExtension p s == LP.addExtension p s
      )
      ,
      ( "hasExtension"
      , property $ \p -> P.hasExtension p == LP.hasExtension p
      )
      ,
      ( "splitExtensions"
      , property $ \p -> P.splitExtensions p == LP.splitExtensions p
      )
      ,
      ( "dropExtensions"
      , property $ \p -> P.dropExtensions p == LP.dropExtensions p
      )
      ,
      ( "takeExtensions"
      , property $ \p -> P.takeExtensions p == LP.takeExtensions p
      )
      ,
      ( "replaceExtensions"
      , property $ \p s -> P.replaceExtensions p s == LP.replaceExtensions p s
      )
      ,
      ( "isExtensionOf"
      , property $ \p s -> P.isExtensionOf p s == LP.isExtensionOf p s
      )
      ,
      ( "stripExtension"
      , property $ \p s -> P.stripExtension p s == LP.stripExtension p s
      )
      ,
      ( "splitFileName"
      , property $ \p -> P.splitFileName p == LP.splitFileName p
      )
      ,
      ( "takeFileName"
      , property $ \p -> P.takeFileName p == LP.takeFileName p
      )
      ,
      ( "replaceFileName"
      , property $ \p s -> P.replaceFileName p s == LP.replaceFileName p s
      )
      ,
      ( "dropFileName"
      , property $ \p -> P.dropFileName p == LP.dropFileName p
      )
      ,
      ( "takeBaseName"
      , property $ \p -> P.takeBaseName p == LP.takeBaseName p
      )
      ,
      ( "replaceBaseName"
      , property $ \p s -> P.replaceBaseName p s == LP.replaceBaseName p s
      )
      ,
      ( "takeDirectory"
      , property $ \p -> P.takeDirectory p == LP.takeDirectory p
      )
      ,
      ( "replaceDirectory"
      , property $ \p s -> P.replaceDirectory p s == LP.replaceDirectory p s
      )
      ,
      ( "combine"
      , property $ \p s -> P.combine p s == LP.combine p s
      )
      ,
      ( "splitPath"
      , property $ \p -> P.splitPath p == LP.splitPath p
      )
      ,
      ( "joinPath"
      , property $ \p -> P.joinPath p == LP.joinPath p
      )
      ,
      ( "splitDirectories"
      , property $ \p -> P.splitDirectories p == LP.splitDirectories p
      )
      ,
      ( "splitDirectories"
      , property $ \p -> P.splitDirectories p == LP.splitDirectories p
      )
      ,
      ( "splitDrive"
      , property $ \p -> P.splitDrive p == LP.splitDrive p
      )
      ,
      ( "joinDrive"
      , property $ \p s -> P.joinDrive p s == LP.joinDrive p s
      )
      ,
      ( "takeDrive"
      , property $ \p -> P.takeDrive p == LP.takeDrive p
      )
      ,
      ( "hasDrive"
      , property $ \p -> P.hasDrive p == LP.hasDrive p
      )
      ,
      ( "dropDrive"
      , property $ \p -> P.dropDrive p == LP.dropDrive p
      )
      ,
      ( "isDrive"
      , property $ \p -> P.isDrive p == LP.isDrive p
      )
      ,
      ( "hasTrailingPathSeparator"
      , property $ \p -> P.hasTrailingPathSeparator p == LP.hasTrailingPathSeparator p
      )
      ,
      ( "addTrailingPathSeparator"
      , property $ \p -> P.addTrailingPathSeparator p == LP.addTrailingPathSeparator p
      )
      ,
      ( "dropTrailingPathSeparator"
      , property $ \p -> P.dropTrailingPathSeparator p == LP.dropTrailingPathSeparator p
      )
      ,
      ( "normalise"
      , property $ \p -> P.normalise p == LP.normalise p
      )
      ,
      ( "equalFilePath"
      , property $ \p s -> P.equalFilePath p s == LP.equalFilePath p s
      )
      ,
      ( "makeRelative"
      , property $ \p s -> P.makeRelative p s == LP.makeRelative p s
      )
      ,
      ( "isRelative"
      , property $ \p -> P.isRelative p == LP.isRelative p
      )
      ,
      ( "isAbsolute"
      , property $ \p -> P.isAbsolute p == LP.isAbsolute p
      )
      ,
      ( "isValid"
      , property $ \p -> P.isValid p == LP.isValid p
      )
      ,
      ( "makeValid"
      , property $ \p -> P.makeValid p == LP.makeValid p
      )
    ]
  ]

