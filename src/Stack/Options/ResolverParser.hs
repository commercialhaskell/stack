{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module Stack.Options.ResolverParser where

import qualified Data.Text                         as T
import           Options.Applicative
import           Options.Applicative.Types         (readerAsk)
import           Stack.Options.Utils
import           Stack.Prelude
import           Stack.Types.Compiler
import           Stack.Types.Resolver

-- | Parser for the resolver
abstractResolverOptsParser :: Bool -> Parser AbstractResolver
abstractResolverOptsParser hide =
    option readAbstractResolver
        (long "resolver" <>
         metavar "RESOLVER" <>
         help "Override resolver in project file" <>
         hideMods hide)

compilerOptsParser :: Bool -> Parser (CompilerVersion 'CVWanted)
compilerOptsParser hide =
    option readCompilerVersion
        (long "compiler" <>
         metavar "COMPILER" <>
         help "Use the specified compiler" <>
         hideMods hide)

readCompilerVersion :: ReadM (CompilerVersion 'CVWanted)
readCompilerVersion = do
    s <- readerAsk
    case parseCompilerVersion (T.pack s) of
        Nothing -> readerError $ "Failed to parse compiler: " ++ s
        Just x -> return x
