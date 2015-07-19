{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TupleSections #-}

-- | Extensions to Aeson parsing of objects.
module Data.Aeson.Extended (
    module Export
  -- * Extended failure messages
  , (.:)
  , (.:?)
  -- * JSON Parser that emits warnings
  , WarningParser
  , JSONWarning (..)
  , withObjectWarnings
  , (..:)
  , (..:?)
  , (..!=)
  , jsonSubWarnings
  , jsonSubWarningsT
  , jsonSubWarningsMT
  , logJSONWarnings
  ) where

import Control.Monad.Logger (MonadLogger, logWarn)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, mapWriterT, runWriterT, tell)
import Data.Aeson as Export hiding ((.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types hiding ((.:), (.:?))
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (Monoid (..), (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack, Text)
import qualified Data.Text as T
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable

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
o ..: k = tellField k >> lift (o .: k)

-- | 'WarningParser' version of @.:?@.
(..:?)
    :: FromJSON a
    => Object -> Text -> WarningParser (Maybe a)
o ..:? k = tellField k >> lift (o .:? k)

-- | 'WarningParser' version of @.!=@.
(..!=) :: WarningParser (Maybe a) -> a -> WarningParser a
wp ..!= d =
    flip mapWriterT wp $
    \p ->
         do a <- fmap snd p
            fmap (, a) (fmap fst p .!= d)

-- | Tell warning parser about about an expected field.
tellField :: Text -> WarningParser ()
tellField key = tell (mempty { wpmExpectedFields = Set.singleton key})

-- | 'MonadParser' version of 'withObject'.
withObjectWarnings :: String
                   -> (Object -> WarningParser a)
                   -> Value
                   -> Parser (a, [JSONWarning])
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
                ( a
                , wpmWarnings w ++
                  case unrecognizedFields of
                      [] -> []
                      _ -> [JSONUnrecognizedFields expected unrecognizedFields])

-- | Log JSON warnings.
logJSONWarnings
    :: MonadLogger m
    => FilePath -> [JSONWarning] -> m ()
logJSONWarnings fp =
    mapM_ (\w -> $logWarn ("Warning: " <> T.pack fp <> ": " <> T.pack (show w)))

-- | Handle warnings in a sub-object.
jsonSubWarnings :: WarningParser (a, [JSONWarning]) -> WarningParser a
jsonSubWarnings f = do
    (result,warnings) <- f
    tell
        (mempty
         { wpmWarnings = warnings
         })
    return result

-- | Handle warnings in a @Traversable@ of sub-objects.
jsonSubWarningsT
    :: Traversable t
    => WarningParser (t (a, [JSONWarning])) -> WarningParser (t a)
jsonSubWarningsT f =
    Traversable.mapM (jsonSubWarnings . return) =<< f

-- | Handle warnings in a @Maybe Traversable@ of sub-objects.
jsonSubWarningsMT
    :: (Traversable t)
    => WarningParser (Maybe (t (a, [JSONWarning])))
    -> WarningParser (Maybe (t a))
jsonSubWarningsMT f = do
    ml <- f
    case ml of
        Nothing -> return Nothing
        Just l -> fmap Just (jsonSubWarningsT (return l))

-- | JSON parser that warns about unexpected fields in objects.
type WarningParser a = WriterT WarningParserMonoid Parser a

-- | Monoid used by 'MonadParser' to track expected fields and warnings.
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

-- | Warning output from 'WarningParser'.
data JSONWarning = JSONUnrecognizedFields String [Text]
instance Show JSONWarning where
    show (JSONUnrecognizedFields obj [field]) =
        "Unrecognized field in " <> obj <> ": " <> T.unpack field
    show (JSONUnrecognizedFields obj fields) =
        "Unrecognized fields in " <> obj <> ": " <> T.unpack (T.intercalate ", " fields)
