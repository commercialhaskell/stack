{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TupleSections #-}

-- | Extensions to Aeson parsing of objects.
module Data.Aeson.Extended (
    module Export
  -- * Extended failure messages
  , (.:)
  , (.:?)
  -- * JSON Parser that emits warnings
  , JSONWarning (..)
  , WarningParser
  , WithJSONWarnings (..)
  , withObjectWarnings
  , jsonSubWarnings
  , jsonSubWarningsT
  , jsonSubWarningsTT
  , logJSONWarnings
  , noJSONWarnings
  , tellJSONField
  , unWarningParser
  , (..:)
  , (..:?)
  , (..!=)
  ) where

import Control.Monad.Logger (MonadLogger, logWarn)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, mapWriterT, runWriterT, tell)
import Data.Aeson as Export hiding ((.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types hiding ((.:), (.:?))
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack, Text)
import qualified Data.Text as T
import Data.Traversable
import qualified Data.Traversable as Traversable
import Prelude -- Fix redundant import warnings

-- | Extends @.:@ warning to include field name.
(.:) :: FromJSON a => Object -> Text -> Parser a
(.:) o p = modifyFailure (("failed to parse field '" <> unpack p <> "': ") <>) (o A..: p)
{-# INLINE (.:) #-}

-- | Extends @.:?@ warning to include field name.
(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) o p = modifyFailure (("failed to parse field '" <> unpack p <> "': ") <>) (o A..:? p)
{-# INLINE (.:?) #-}

-- | 'WarningParser' version of @.:@.
(..:)
    :: FromJSON a
    => Object -> Text -> WarningParser a
o ..: k = tellJSONField k >> lift (o .: k)

-- | 'WarningParser' version of @.:?@.
(..:?)
    :: FromJSON a
    => Object -> Text -> WarningParser (Maybe a)
o ..:? k = tellJSONField k >> lift (o .:? k)

-- | 'WarningParser' version of @.!=@.
(..!=) :: WarningParser (Maybe a) -> a -> WarningParser a
wp ..!= d =
    flip mapWriterT wp $
    \p ->
         do a <- fmap snd p
            fmap (, a) (fmap fst p .!= d)

-- | Tell warning parser about an expected field, so it doesn't warn about it.
tellJSONField :: Text -> WarningParser ()
tellJSONField key = tell (mempty { wpmExpectedFields = Set.singleton key})

-- | 'WarningParser' version of 'withObject'.
withObjectWarnings :: String
                   -> (Object -> WarningParser a)
                   -> Value
                   -> Parser (WithJSONWarnings a)
withObjectWarnings expected f =
    withObject expected $
    \obj ->
         do (a,w) <- runWriterT (f obj)
            let unrecognizedFields =
                    Set.toList
                        (Set.difference
                             (Set.fromList (HashMap.keys obj))
                             (wpmExpectedFields w))
            return
                (WithJSONWarnings a
                    (wpmWarnings w ++
                     case unrecognizedFields of
                         [] -> []
                         _ -> [JSONUnrecognizedFields expected unrecognizedFields]))

-- | Convert a 'WarningParser' to a 'Parser'.
unWarningParser :: WarningParser a -> Parser a
unWarningParser wp = do
    (a,_) <- runWriterT wp
    return a

-- | Log JSON warnings.
logJSONWarnings
    :: MonadLogger m
    => FilePath -> [JSONWarning] -> m ()
logJSONWarnings fp =
    mapM_ (\w -> $logWarn ("Warning: " <> T.pack fp <> ": " <> T.pack (show w)))

-- | Handle warnings in a sub-object.
jsonSubWarnings :: WarningParser (WithJSONWarnings a) -> WarningParser a
jsonSubWarnings f = do
    WithJSONWarnings result warnings <- f
    tell
        (mempty
         { wpmWarnings = warnings
         })
    return result

-- | Handle warnings in a @Traversable@ of sub-objects.
jsonSubWarningsT
    :: Traversable t
    => WarningParser (t (WithJSONWarnings a)) -> WarningParser (t a)
jsonSubWarningsT f =
    Traversable.mapM (jsonSubWarnings . return) =<< f

-- | Handle warnings in a @Maybe Traversable@ of sub-objects.
jsonSubWarningsTT
    :: (Traversable t, Traversable u)
    => WarningParser (u (t (WithJSONWarnings a)))
    -> WarningParser (u (t a))
jsonSubWarningsTT f =
    Traversable.mapM (jsonSubWarningsT . return) =<< f

-- Parsed JSON value without any warnings
noJSONWarnings :: a -> WithJSONWarnings a
noJSONWarnings v = WithJSONWarnings v []

-- | JSON parser that warns about unexpected fields in objects.
type WarningParser a = WriterT WarningParserMonoid Parser a

-- | Monoid used by 'WarningParser' to track expected fields and warnings.
data WarningParserMonoid = WarningParserMonoid
    { wpmExpectedFields :: !(Set Text)
    , wpmWarnings :: [JSONWarning]
    }
instance Monoid WarningParserMonoid where
    mempty = WarningParserMonoid Set.empty []
    mappend a b =
        WarningParserMonoid
        { wpmExpectedFields = Set.union
              (wpmExpectedFields a)
              (wpmExpectedFields b)
        , wpmWarnings = wpmWarnings a ++ wpmWarnings b
        }

-- Parsed JSON value with its warnings
data WithJSONWarnings a = WithJSONWarnings a [JSONWarning]
instance Functor WithJSONWarnings where
    fmap f (WithJSONWarnings x w) = WithJSONWarnings (f x) w
instance Monoid a => Monoid (WithJSONWarnings a) where
    mempty = noJSONWarnings mempty
    mappend (WithJSONWarnings a aw) (WithJSONWarnings b bw) = WithJSONWarnings (mappend a b) (mappend aw bw)

-- | Warning output from 'WarningParser'.
data JSONWarning = JSONUnrecognizedFields String [Text]
instance Show JSONWarning where
    show (JSONUnrecognizedFields obj [field]) =
        "Unrecognized field in " <> obj <> ": " <> T.unpack field
    show (JSONUnrecognizedFields obj fields) =
        "Unrecognized fields in " <> obj <> ": " <> T.unpack (T.intercalate ", " fields)
