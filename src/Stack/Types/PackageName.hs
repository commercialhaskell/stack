{-# LANGUAGE NoImplicitPrelude #-}

-- | Names for packages.

module Stack.Types.PackageName
  ( packageNameArgument
  ) where

import qualified Options.Applicative as O
import           Stack.Prelude

-- | An argument which accepts a template name of the format
-- @foo.hsfiles@.
packageNameArgument ::
     O.Mod O.ArgumentFields PackageName
  -> O.Parser PackageName
packageNameArgument =
  O.argument
    (do s <- O.str
        either O.readerError pure (p s))
 where
  p s =
    case parsePackageName s of
      Just x -> Right x
      Nothing -> Left $ unlines
        [ "Expected a package name acceptable to Cabal, but got: " ++ s ++ "\n"
        , "An acceptable package name comprises an alphanumeric 'word'; or \
          \two or more"
        , "such words, with the words separated by a hyphen/minus character ('-'). A \
          \word"
        , "cannot be comprised only of the characters '0' to '9'. \n"
        , "An alphanumeric character is one in one of the Unicode Letter \
          \categories"
        , "(Lu (uppercase), Ll (lowercase), Lt (titlecase), Lm (modifier), or \
          \Lo (other))"
        , "or Number categories (Nd (decimal), Nl (letter), or No (other))."
        ]
