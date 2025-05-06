{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.PackagesParser
Description : Parser for one or more package names.
License     : BSD-3-Clause

Parser for one or more package names.
-}

module Stack.Options.PackagesParser
  ( packagesParser
  ) where

import           Options.Applicative ( Parser, help, long, metavar, strOption )
import           Stack.Prelude

-- | Parser for one or more package names.
packagesParser :: Parser [String]
packagesParser = many
  ( strOption
      (  long "package"
      <> metavar "PACKAGE"
      <> help "Add a package (can be specified multiple times)"
      )
  )
