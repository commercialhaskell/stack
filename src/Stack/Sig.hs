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

module Stack.Sig (module Sig, signOpts) where

import Options.Applicative
import Stack.Sig.GPG as Sig
import Stack.Sig.Sign as Sig

-- | Options for commands that sign packages
signOpts :: Parser (Bool, String)
signOpts =
    (,) <$>
    switch (long "no-signature" <> help "Do not sign & upload signatures") <*>
    strOption
        (long "sig-server" <> metavar "URL" <> showDefault <>
         value "https://sig.commercialhaskell.org" <>
         help "URL")
