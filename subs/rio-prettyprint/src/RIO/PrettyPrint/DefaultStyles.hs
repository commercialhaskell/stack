{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RIO.PrettyPrint.DefaultStyles
  (
    defaultStyles
  ) where

import Data.Array.IArray (array)
import RIO
import RIO.PrettyPrint.Types (Style (..), Styles)
import System.Console.ANSI.Codes (Color (..), ColorIntensity (..),
  ConsoleIntensity (..), ConsoleLayer (..), SGR (..))

-- | Default styles for rio-prettyprint output.
defaultStyles :: Styles
defaultStyles = array (minBound, maxBound)
  [ (Error, ("error", [SetColor Foreground Vivid Red]))
  , (Warning, ("warning", [SetColor Foreground Dull Yellow]))
  , (Good, ("good", [SetColor Foreground Vivid Green]))
  , (Shell, ("shell", [SetColor Foreground Vivid Magenta]))
  , (File, ("file", [SetColor Foreground Dull Cyan]))
  -- For now 'Url' using the same style as 'File'
  , (Url, ("url", [SetColor Foreground Dull Cyan]))
  , (Dir, ("dir", [ SetConsoleIntensity BoldIntensity
                  , SetColor Foreground Vivid Blue ]))
  , (Recommendation, ("recommendation", [ SetConsoleIntensity BoldIntensity
                                      , SetColor Foreground Vivid Green]))
  , (Current, ("current", [SetColor Foreground Dull Yellow]))
  , (Target, ("target", [SetColor Foreground Vivid Cyan]))
  -- TODO: what color should Module be?
  , (Module, ("module", [SetColor Foreground Vivid Magenta]))
  , (PkgComponent, ("package-component", [SetColor Foreground Vivid Cyan])) ]
