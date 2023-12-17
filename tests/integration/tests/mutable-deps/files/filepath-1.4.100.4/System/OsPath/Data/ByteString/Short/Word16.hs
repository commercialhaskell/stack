{- HLINT ignore -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing -fexpose-all-unfoldings #-}

-- |
-- Module      :  System.OsPath.Data.ByteString.Short.Word16
-- Copyright   :  © 2022 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- ShortByteStrings encoded as UTF16-LE, suitable for windows FFI calls.
--
-- Word16s are *always* in BE encoding (both input and output), so e.g. 'pack'
-- takes a list of BE encoded @[Word16]@ and produces a UTF16-LE encoded ShortByteString.
--
-- Likewise, 'unpack' takes a UTF16-LE encoded ShortByteString and produces a list of BE encoded @[Word16]@.
--
-- Indices and lengths are always in respect to Word16, not Word8.
--
-- All functions will error out if the input string is not a valid UTF16 stream (uneven number of bytes).
-- So use this module with caution.
module System.OsPath.Data.ByteString.Short.Word16 (
    -- * The @ShortByteString@ type and representation
    ShortByteString(..),

    -- * Introducing and eliminating 'ShortByteString's
    empty,
    singleton,
    pack,
    unpack,
    fromShort,
    toShort,

    -- * Basic interface
    snoc,
    cons,
    append,
    last,
    tail,
    uncons,
    uncons2,
    head,
    init,
    unsnoc,
    null,
    length,
    numWord16,

    -- * Transforming ShortByteStrings
    map,
    reverse,
    intercalate,

    -- * Reducing 'ShortByteString's (folds)
    foldl,
    foldl',
    foldl1,
    foldl1',

    foldr,
    foldr',
    foldr1,
    foldr1',

    -- ** Special folds
    all,
    any,
    concat,

    -- ** Generating and unfolding ByteStrings
    replicate,
    unfoldr,
    unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    takeWhileEnd,
    takeWhile,
    drop,
    dropEnd,
    dropWhile,
    dropWhileEnd,
    breakEnd,
    break,
    span,
    spanEnd,
    splitAt,
    split,
    splitWith,
    stripSuffix,
    stripPrefix,

    -- * Predicates
    isInfixOf,
    isPrefixOf,
    isSuffixOf,

    -- ** Search for arbitrary substrings
    breakSubstring,

    -- * Searching ShortByteStrings

    -- ** Searching by equality
    elem,

    -- ** Searching with a predicate
    find,
    filter,
    partition,

    -- * Indexing ShortByteStrings
    index,
    indexMaybe,
    (!?),
    elemIndex,
    elemIndices,
    count,
    findIndex,
    findIndices,

    -- ** Encoding validation
    -- isValidUtf8,

    -- * Low level conversions
    -- ** Packing 'CString's and pointers
    packCWString,
    packCWStringLen,
    newCWString,

    -- ** Using ShortByteStrings as 'CString's
    useAsCWString,
    useAsCWStringLen
  )
where
import System.OsPath.Data.ByteString.Short ( append, intercalate, concat, stripSuffix, stripPrefix, isPrefixOf, isSuffixOf, length, empty, null, ShortByteString(..), fromShort, toShort )
import System.OsPath.Data.ByteString.Short.Internal
import Data.Bits
    ( shiftR
    )
import Data.Word
import Prelude hiding
    ( Foldable(..)
    , all
    , any
    , reverse
    , break
    , concat
    , drop
    , dropWhile
    , filter
    , head
    , init
    , last
    , map
    , replicate
    , span
    , splitAt
    , tail
    , take
    , takeWhile
    )
import qualified Data.Foldable as Foldable
import GHC.ST ( ST )
import GHC.Stack ( HasCallStack )
import GHC.Exts ( inline )

import qualified Data.ByteString.Short.Internal as BS
import qualified Data.List as List


-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ShortByteString's

-- | /O(1)/ Convert a 'Word16' into a 'ShortByteString'
singleton :: Word16 -> ShortByteString
singleton = \w -> create 2 (\mba -> writeWord16Array mba 0 w)


-- | /O(n)/. Convert a list into a 'ShortByteString'
pack :: [Word16] -> ShortByteString
pack = packWord16


-- | /O(n)/. Convert a 'ShortByteString' into a list.
unpack :: ShortByteString -> [Word16]
unpack = unpackWord16 . assertEven


-- ---------------------------------------------------------------------
-- Basic interface

-- | This is like 'length', but the number of 'Word16', not 'Word8'.
numWord16 :: ShortByteString -> Int
numWord16 = (`shiftR` 1) . BS.length . assertEven

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ Append a Word16 to the end of a 'ShortByteString'
--
-- Note: copies the entire byte array
snoc :: ShortByteString -> Word16 -> ShortByteString
snoc = \(assertEven -> sbs) c -> let l = BS.length sbs
                                     nl = l + 2
  in create nl $ \mba -> do
      copyByteArray (asBA sbs) 0 mba 0 l
      writeWord16Array mba l c

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
-- Note: copies the entire byte array
cons :: Word16 -> ShortByteString -> ShortByteString
cons c = \(assertEven -> sbs) -> let l = BS.length sbs
                                     nl = l + 2
  in create nl $ \mba -> do
      writeWord16Array mba 0 c
      copyByteArray (asBA sbs) 0 mba 2 l

-- | /O(1)/ Extract the last element of a ShortByteString, which must be finite and at least one Word16.
-- An exception will be thrown in the case of an empty ShortByteString.
last :: HasCallStack => ShortByteString -> Word16
last = \(assertEven -> sbs) -> case null sbs of
  True -> errorEmptySBS "last"
  False -> indexWord16Array (asBA sbs) (BS.length sbs - 2)

-- | /O(n)/ Extract the elements after the head of a ShortByteString, which must at least one Word16.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- Note: copies the entire byte array
tail :: HasCallStack => ShortByteString -> ShortByteString
tail = \(assertEven -> sbs) ->
  let l = BS.length sbs
      nl = l - 2
  in if
      | l <= 0 -> errorEmptySBS "tail"
      | otherwise -> create nl $ \mba -> copyByteArray (asBA sbs) 2 mba 0 nl

-- | /O(n)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ShortByteString -> Maybe (Word16, ShortByteString)
uncons = \(assertEven -> sbs) ->
  let l  = BS.length sbs
      nl = l - 2
  in if | l <= 0 -> Nothing
        | otherwise -> let h = indexWord16Array (asBA sbs) 0
                           t = create nl $ \mba -> copyByteArray (asBA sbs) 2 mba 0 nl
                       in Just (h, t)

-- | /O(n)/ Extract first two elements and the rest of a ByteString,
-- returning Nothing if it is shorter than two elements.
uncons2 :: ShortByteString -> Maybe (Word16, Word16, ShortByteString)
uncons2 = \(assertEven -> sbs) ->
  let l  = BS.length sbs
      nl = l - 4
  in if | l <= 2 -> Nothing
        | otherwise -> let h  = indexWord16Array (asBA sbs) 0
                           h' = indexWord16Array (asBA sbs) 2
                           t  = create nl $ \mba -> copyByteArray (asBA sbs) 4 mba 0 nl
                       in Just (h, h', t)

-- | /O(1)/ Extract the first element of a ShortByteString, which must be at least one Word16.
-- An exception will be thrown in the case of an empty ShortByteString.
head :: HasCallStack => ShortByteString -> Word16
head = \(assertEven -> sbs) -> case null sbs of
  True -> errorEmptySBS "last"
  False -> indexWord16Array (asBA sbs) 0

-- | /O(n)/ Return all the elements of a 'ShortByteString' except the last one.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- Note: copies the entire byte array
init :: HasCallStack => ShortByteString -> ShortByteString
init = \(assertEven -> sbs) ->
  let l = BS.length sbs
      nl = l - 2
  in if
      | l <= 0 -> errorEmptySBS "tail"
      | otherwise   -> create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl

-- | /O(n)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- if it is empty.
unsnoc :: ShortByteString -> Maybe (ShortByteString, Word16)
unsnoc = \(assertEven -> sbs) ->
  let l  = BS.length sbs
      nl = l - 2
  in if | l <= 0 -> Nothing
        | otherwise -> let l' = indexWord16Array (asBA sbs) (l - 2)
                           i  = create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl
                       in Just (i, l')


-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ShortByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word16 -> Word16) -> ShortByteString -> ShortByteString
map f = \(assertEven -> sbs) ->
    let l = BS.length sbs
        ba = asBA sbs
    in create l (\mba -> go ba mba 0 l)
  where
    go :: BA -> MBA s -> Int -> Int -> ST s ()
    go !ba !mba !i !l
      | i >= l = return ()
      | otherwise = do
          let w = indexWord16Array ba i
          writeWord16Array mba i (f w)
          go ba mba (i+2) l

-- TODO: implement more efficiently
-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ShortByteString -> ShortByteString
reverse = \(assertEven -> sbs) ->
    let l = BS.length sbs
        ba = asBA sbs
    in create l (\mba -> go ba mba 0 l)
  where
    go :: BA -> MBA s -> Int -> Int -> ST s ()
    go !ba !mba !i !l
      | i >= l = return ()
      | otherwise = do
          let w = indexWord16Array ba i
          writeWord16Array mba (l - 2 - i) w
          go ba mba (i+2) l


-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Applied to a predicate and a 'ShortByteString', 'all' determines
-- if all elements of the 'ShortByteString' satisfy the predicate.
all :: (Word16 -> Bool) -> ShortByteString -> Bool
all k = \(assertEven -> sbs) ->
  let l = BS.length sbs
      ba = asBA sbs
      w = indexWord16Array ba
      go !n | n >= l = True
            | otherwise = k (w n) && go (n + 2)
  in go 0


-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word16 -> Bool) -> ShortByteString -> Bool
any k = \(assertEven -> sbs) ->
  let l = BS.length sbs
      ba = asBA sbs
      w = indexWord16Array ba
      go !n | n >= l = False
            | otherwise = k (w n) || go (n + 2)
  in go 0


-- ---------------------------------------------------------------------
-- Unfolds and replicates


-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
replicate :: Int -> Word16 -> ShortByteString
replicate w c
    | w <= 0    = empty
    -- can't use setByteArray here, because we write UTF-16LE
    | otherwise = create (w * 2) (`go` 0)
  where
    go mba ix
      | ix < 0 || ix >= w * 2 = pure ()
      | otherwise = writeWord16Array mba ix c >> go mba (ix + 2)

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ShortByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ShortByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- This function is not efficient/safe. It will build a list of @[Word16]@
-- and run the generator until it returns `Nothing`, otherwise recurse infinitely,
-- then finally create a 'ShortByteString'.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word16, a)) -> a -> ShortByteString
unfoldr f x0 = packWord16Rev $ go x0 mempty
 where
   go x words' = case f x of
                    Nothing -> words'
                    Just (w, x') -> go x' (w:words')

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ShortByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: forall a.
            Int  -- ^ number of 'Word16'
         -> (a -> Maybe (Word16, a))
         -> a
         -> (ShortByteString, Maybe a)
unfoldrN i f = \x0 ->
  if | i < 0     -> (empty, Just x0)
     | otherwise -> createAndTrim (i * 2) $ \mba -> go mba x0 0

  where
    go :: forall s. MBA s -> a -> Int -> ST s (Int, Maybe a)
    go !mba !x !n = go' x n
      where
        go' :: a -> Int -> ST s (Int, Maybe a)
        go' !x' !n'
          | n' == i * 2 = return (n', Just x')
          | otherwise   = case f x' of
                          Nothing       -> return (n', Nothing)
                          Just (w, x'') -> do
                                             writeWord16Array mba n' w
                                             go' x'' (n'+2)


-- --------------------------------------------------------------------
-- Predicates



-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n)/ 'take' @n@, applied to a ShortByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- Note: copies the entire byte array
take :: Int  -- ^ number of Word16
     -> ShortByteString
     -> ShortByteString
take = \n (assertEven -> sbs) ->
                     let sl   = numWord16 sbs
                         len8 = n * 2
                     in if | n >= sl   -> sbs
                           | n <= 0    -> empty
                           | otherwise ->
                               create len8 $ \mba -> copyByteArray (asBA sbs) 0 mba 0 len8


-- | /O(1)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "a\NULb\NULc\NULd\NULe\NULf\NULg\NUL"
-- "e\NULf\NULg\NUL"
-- >>> takeEnd 0 "a\NULb\NULc\NULd\NULe\NULf\NULg\NUL"
-- ""
-- >>> takeEnd 4 "a\NULb\NULc\NUL"
-- "a\NULb\NULc\NUL"
takeEnd :: Int  -- ^ number of 'Word16'
        -> ShortByteString
        -> ShortByteString
takeEnd n = \(assertEven -> sbs) ->
                    let sl = BS.length sbs
                        n2 = n * 2
                    in if | n2 >= sl  -> sbs
                          | n2 <= 0   -> empty
                          | otherwise -> create n2 $ \mba -> copyByteArray (asBA sbs) (max 0 (sl - n2)) mba 0 n2

-- | Similar to 'P.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
takeWhile f ps = take (findIndexOrLength (not . f) ps) ps

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
takeWhileEnd :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
takeWhileEnd f ps = drop (findFromEndUntil (not . f) ps) ps


-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or @[]@ if @n > 'length' xs@.
--
-- Note: copies the entire byte array
drop  :: Int  -- ^ number of 'Word16'
      -> ShortByteString
      -> ShortByteString
drop = \n' (assertEven -> sbs) ->
  let len = BS.length sbs
      n   = n' * 2
  in if | n <= 0    -> sbs
        | n >= len  -> empty
        | otherwise ->
            let newLen = len - n
            in create newLen $ \mba -> copyByteArray (asBA sbs) n mba 0 newLen

-- | /O(1)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "a\NULb\NULc\NULd\NULe\NULf\NULg\NUL"
-- "a\NULb\NULc\NULd\NUL"
-- >>> dropEnd 0 "a\NULb\NULc\NULd\NULe\NULf\NULg\NUL"
-- "a\NULb\NULc\NULd\NULe\NULf\NULg\NUL"
-- >>> dropEnd 4 "a\NULb\NULc\NUL"
-- ""
dropEnd :: Int  -- ^ number of 'Word16'
        -> ShortByteString
        -> ShortByteString
dropEnd n' = \(assertEven -> sbs) ->
                    let sl = BS.length sbs
                        nl = sl - n
                        n  = n' * 2
                    in if | n >= sl   -> empty
                          | n <= 0    -> sbs
                          | otherwise -> create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl

-- | Similar to 'P.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
--
-- Note: copies the entire byte array
dropWhile :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
dropWhile f = \(assertEven -> ps) -> drop (findIndexOrLength (not . f) ps) ps

-- | Similar to 'P.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
dropWhileEnd f = \(assertEven -> ps) -> take (findFromEndUntil (not . f) ps) ps

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
breakEnd :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
breakEnd p = \(assertEven -> sbs) -> splitAt (findFromEndUntil p sbs) sbs

-- | Similar to 'P.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
break :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
break = \p (assertEven -> ps) -> case findIndexOrLength p ps of n -> splitAt n ps

-- | Similar to 'P.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
{- HLINT ignore "Use span" -}
span p = break (not . p) . assertEven

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('takeWhileEnd' p &&& 'dropWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse ps) in (reverse y, reverse x)
--
spanEnd :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
spanEnd  p = \(assertEven -> ps) -> splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
--
-- Note: copies the substrings
splitAt :: Int -- ^ number of Word16
        -> ShortByteString
        -> (ShortByteString, ShortByteString)
splitAt n' = \(assertEven -> sbs) -> if
  | n <= 0 -> (empty, sbs)
  | otherwise ->
      let slen = BS.length sbs
      in if | n >= BS.length sbs -> (sbs, empty)
            | otherwise ->
                let llen = min slen (max 0 n)
                    rlen = max 0 (slen - max 0 n)
                    lsbs = create llen $ \mba -> copyByteArray (asBA sbs) 0 mba 0 llen
                    rsbs = create rlen $ \mba -> copyByteArray (asBA sbs) n mba 0 rlen
                in (lsbs, rsbs)
 where
  n = n' * 2

-- | /O(n)/ Break a 'ShortByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- Note: copies the substrings
split :: Word16 -> ShortByteString -> [ShortByteString]
split w = splitWith (== w) . assertEven


-- | /O(n)/ Splits a 'ShortByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word16 -> Bool) -> ShortByteString -> [ShortByteString]
splitWith p = \(assertEven -> sbs) -> if
  | BS.null sbs -> []
  | otherwise -> go sbs
  where
    go sbs'
      | BS.null sbs' = [mempty]
      | otherwise =
          case break p sbs' of
            (a, b)
              | BS.null b -> [a]
              | otherwise -> a : go (tail b)


-- | Check whether one string is a substring of another.
isInfixOf :: ShortByteString -> ShortByteString -> Bool
isInfixOf sbs = \s -> null sbs || not (null $ snd $ GHC.Exts.inline breakSubstring sbs s)


-- algorithm: https://github.com/haskell/filepath/issues/195#issuecomment-1605633713
breakSubstring :: ShortByteString -- ^ String to search for
               -> ShortByteString -- ^ String to search in
               -> (ShortByteString, ShortByteString) -- ^ Head and tail of string broken at substring
breakSubstring bPat@(asBA -> pat) bInp@(asBA -> inp) = go 0
 where
    lpat = BS.length bPat
    linp = BS.length bInp
    go ix
      | let ix' = ix * 2
      , linp >= ix' + lpat =
          if | compareByteArraysOff pat 0 inp ix' lpat == 0 -> splitAt ix bInp
             | otherwise -> go (ix + 1)
      | otherwise
      = (bInp, mempty)


-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ShortByteString, reduces the
-- ShortByteString using the binary operator, from left to right.
--
foldl :: (a -> Word16 -> a) -> a -> ShortByteString -> a
foldl f v = List.foldl f v . unpack . assertEven

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word16 -> a) -> a -> ShortByteString -> a
foldl' f v = List.foldl' f v . unpack . assertEven

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ShortByteString,
-- reduces the ShortByteString using the binary operator, from right to left.
foldr :: (Word16 -> a -> a) -> a -> ShortByteString -> a
foldr f v = List.foldr f v . unpack . assertEven

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
foldr' :: (Word16 -> a -> a) -> a -> ShortByteString -> a
foldr' k v = Foldable.foldr' k v . unpack . assertEven

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ShortByteString's.
-- An exception will be thrown in the case of an empty ShortByteString.
foldl1 :: HasCallStack => (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldl1 k = List.foldl1 k . unpack . assertEven

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty ShortByteString.
foldl1' :: HasCallStack => (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldl1' k = List.foldl1' k . unpack . assertEven

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ShortByteString's
-- An exception will be thrown in the case of an empty ShortByteString.
foldr1 :: HasCallStack => (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldr1 k = List.foldr1 k . unpack . assertEven

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: HasCallStack => (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldr1' k = \(assertEven -> sbs) -> if null sbs then errorEmptySBS "foldr1'" else foldr' k (last sbs) (init sbs)


-- --------------------------------------------------------------------
-- Searching ShortByteString

-- | /O(1)/ 'ShortByteString' index (subscript) operator, starting from 0.
index :: HasCallStack
      => ShortByteString
      -> Int  -- ^ number of 'Word16'
      -> Word16
index = \(assertEven -> sbs) i -> if
  | i >= 0 && i < numWord16 sbs -> unsafeIndex sbs i
  | otherwise                   -> indexError sbs i

-- | /O(1)/ 'ShortByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ShortByteString
           -> Int  -- ^ number of 'Word16'
           -> Maybe Word16
indexMaybe = \(assertEven -> sbs) i -> if
  | i >= 0 && i < numWord16 sbs -> Just $! unsafeIndex sbs i
  | otherwise                   -> Nothing
{-# INLINE indexMaybe #-}

unsafeIndex :: ShortByteString
            -> Int  -- ^ number of 'Word16'
            -> Word16
unsafeIndex sbs i = indexWord16Array (asBA sbs) (i * 2)

indexError :: HasCallStack => ShortByteString -> Int -> a
indexError sbs i =
  moduleError "index" $ "error in array index: " ++ show i
                        ++ " not in range [0.." ++ show (numWord16 sbs) ++ "]"

-- | /O(1)/ 'ShortByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ShortByteString
     -> Int  -- ^ number of 'Word16'
     -> Maybe Word16
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ 'elem' is the 'ShortByteString' membership predicate.
elem :: Word16 -> ShortByteString -> Bool
elem c = \(assertEven -> sbs) -> case elemIndex c sbs of Nothing -> False ; _ -> True

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
filter k = \(assertEven -> sbs) ->
                   let l = BS.length sbs
                   in if | l <= 0    -> sbs
                         | otherwise -> createAndTrim' l $ \mba -> go mba (asBA sbs) l
  where
    go :: forall s. MBA s -- mutable output bytestring
       -> BA              -- input bytestring
       -> Int             -- length of input bytestring
       -> ST s Int
    go !mba ba !l = go' 0 0
      where
        go' :: Int -- bytes read
            -> Int -- bytes written
            -> ST s Int
        go' !br !bw
          | br >= l   = return bw
          | otherwise = do
              let w = indexWord16Array ba br
              if k w
              then do
                writeWord16Array mba bw w
                go' (br+2) (bw+2)
              else
                go' (br+2) bw

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word16 -> Bool) -> ShortByteString -> Maybe Word16
find f = \(assertEven -> sbs) -> case findIndex f sbs of
                    Just n -> Just (sbs `index` n)
                    _      -> Nothing

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
partition k = \(assertEven -> sbs) ->
                   let l = BS.length sbs
                   in if | l <= 0    -> (sbs, sbs)
                         | otherwise -> createAndTrim'' l $ \mba1 mba2 -> go mba1 mba2 (asBA sbs) l
  where
    go :: forall s.
          MBA s           -- mutable output bytestring1
       -> MBA s           -- mutable output bytestring2
       -> BA              -- input bytestring
       -> Int             -- length of input bytestring
       -> ST s (Int, Int) -- (length mba1, length mba2)
    go !mba1 !mba2 ba !l = go' 0 0
      where
        go' :: Int -- bytes read
            -> Int -- bytes written to bytestring 1
            -> ST s (Int, Int) -- (length mba1, length mba2)
        go' !br !bw1
          | br >= l   = return (bw1, br - bw1)
          | otherwise = do
              let w = indexWord16Array ba br
              if k w
              then do
                writeWord16Array mba1 bw1 w
                go' (br+2) (bw1+2)
              else do
                writeWord16Array mba2 (br - bw1) w
                go' (br+2) bw1

-- --------------------------------------------------------------------
-- Indexing ShortByteString

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ShortByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Word16
          -> ShortByteString
          -> Maybe Int  -- ^ number of 'Word16'
{- HLINT ignore "Use elemIndex" -}
elemIndex k = findIndex (==k) . assertEven

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Word16 -> ShortByteString -> [Int]
{- HLINT ignore "Use elemIndices" -}
elemIndices k = findIndices (==k) . assertEven

-- | count returns the number of times its argument appears in the ShortByteString
count :: Word16 -> ShortByteString -> Int
count w = List.length . elemIndices w . assertEven

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'ShortByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word16 -> Bool) -> ShortByteString -> Maybe Int
findIndex k = \(assertEven -> sbs) ->
  let l = BS.length sbs
      ba = asBA sbs
      w = indexWord16Array ba
      go !n | n >= l    = Nothing
            | k (w n)   = Just (n `shiftR` 1)
            | otherwise = go (n + 2)
  in go 0

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word16 -> Bool) -> ShortByteString -> [Int]
findIndices k = \(assertEven -> sbs) ->
  let l = BS.length sbs
      ba = asBA sbs
      w = indexWord16Array ba
      go !n | n >= l    = []
            | k (w n)   = (n `shiftR` 1) : go (n + 2)
            | otherwise = go (n + 2)
  in go 0
