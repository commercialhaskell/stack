-- |
-- Module      : Acme.Dont
-- Copyright   : Gracjan Polak 2009
-- License     : BSD-style
-- Maintainer  : Gracjan Polak <gracjanpolak@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- The Acme.Dont module provides the indispensable don't command,
-- ported from Perl.
--
-- For more information see influential documentation:
-- <http://search.cpan.org/~dconway/Acme-Don-t-1.01/t.pm>
--
-- Usage:
--
-- > main = don't $ do
-- >     name <- getLine
-- >     putStrLn $ "hello " ++ name
--
module Acme.Dont where

-- | The Acme.Dont module provides a don't command, which is the
-- opposite of Haskell's built-in do.  It is used exactly like the do
-- monadic construct except that, instead of executing the block it
-- controls, it... well... doesn't.
--
-- Regardless of the contents of the block, don't returns ().
--
don't :: (Monad m) => m a -> m ()
don't _action = pure ()
