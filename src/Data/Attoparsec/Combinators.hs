-- | More readable combinators for writing parsers.

module Data.Attoparsec.Combinators where

import Control.Applicative
import Data.Monoid

-- | Concatenate two parsers.
appending :: (Applicative f,Monoid a)
                 => f a -> f a -> f a
appending a b = (<>) <$> a <*> b

-- | Alternative parsers.
alternating :: Alternative f
            => f a -> f a -> f a
alternating a b = a <|> b

-- | Pure something.
pured :: (Applicative g,Applicative f) => g a -> g (f a)
pured = fmap pure

-- | Concatting the result of an action.
concating :: (Monoid m,Applicative f) => f [m] -> f m
concating = fmap mconcat
