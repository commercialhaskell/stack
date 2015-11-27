{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Stack.Sig
Description : GPG Signatures for Stack
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig
       ( module Sig
       , sigCmdName
       , sigSignCmdName
       , sigSignHackageCmdName
       , sigSignHackageOpts
       , sigSignSdistCmdName
       , sigSignSdistOpts
       )
       where

import Options.Applicative
import Stack.Sig.GPG as Sig
import Stack.Sig.Sign as Sig

-- | The command name for dealing with signatures.
sigCmdName :: String
sigCmdName = "sig"

-- | The command name for signing packages.
sigSignCmdName :: String
sigSignCmdName = "sign"

-- | The command name for signing an sdist package file.
sigSignSdistCmdName :: String
sigSignSdistCmdName = "sdist"

-- | The command name for signing all your packages from hackage.org.
sigSignHackageCmdName :: String
sigSignHackageCmdName = "hackage"

-- | The URL of the running signature service to use (sig-service)
url :: Parser String
url = strOption
        (long "url" <>
         short 'u' <>
         metavar "URL" <>
         showDefault <>
         value "https://sig.commercialhaskell.org")

-- | Signature sign (sdist) options
sigSignSdistOpts :: Parser (String, String)
sigSignSdistOpts = helper <*>
    ((,) <$> url <*>
     argument str (metavar "PATH"))

-- | Signature sign (hackage) options
sigSignHackageOpts :: Parser (String, String)
sigSignHackageOpts = helper <*>
    ((,) <$> url <*>
     argument str (metavar "USER"))
