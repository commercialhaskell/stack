{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Gen where

import System.FilePath
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics
import Generic.Random
import Generics.Deriving.Show
import Prelude as P
import Test.Tasty.QuickCheck hiding ((==>))

import qualified Data.List.NonEmpty as NE


class AltShow a where
  altShow :: a -> String

instance {-# OVERLAPPABLE #-} Show a => AltShow a where
  altShow = show

instance {-# OVERLAPS #-} AltShow String where
  altShow = id

instance {-# OVERLAPPABLE #-} AltShow a => AltShow (Maybe a) where
  altShow Nothing = ""
  altShow (Just a) = altShow a


newtype WindowsFilePaths = WindowsFilePaths { unWindowsFilePaths :: [WindowsFilePath] }
  deriving (Show, Eq, Ord, Generic)

-- filepath = namespace *"\" namespace-tail
--          / UNC
--          / [ disk ] *"\" relative-path
--          / disk *"\"
data WindowsFilePath = NS NameSpace [Separator] NSTail
                     | UNC UNCShare
                     | N (Maybe Char) [Separator] (Maybe RelFilePath)
                     -- ^ This differs from the grammar, because we allow
                     -- empty paths
                     | PotentiallyInvalid FilePath
                     -- ^ this branch is added purely for the tests
  deriving (GShow, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[6, 2, 2, 1] `AndShrinking` WindowsFilePath)

instance Show WindowsFilePath where
  show wf = gshow wf ++ " (" ++ altShow wf ++ ")"

instance AltShow WindowsFilePath where
  altShow (NS ns seps nstail) = altShow ns ++ altShow seps ++ altShow nstail
  altShow (UNC unc) = altShow unc
  altShow (N mdisk seps mfrp) = maybe [] (:[]) mdisk ++ (altShow seps ++ altShow mfrp)
  altShow (PotentiallyInvalid fp) = fp


-- namespace-tail     = ( disk 1*"\" relative-path ; C:foo\bar is not valid
--                                                 ; namespaced paths are all absolute
--                      / disk *"\"
--                      / relative-path
--                      )
data NSTail = NST1 Char (NonEmpty Separator) RelFilePath
            | NST2 Char [Separator]
            | NST3 RelFilePath
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[1, 1, 1] `AndShrinking` NSTail)

instance AltShow NSTail where
  altShow (NST1 disk seps relfp) = disk:':':(altShow seps ++ altShow relfp)
  altShow (NST2 disk seps) = disk:':':altShow seps
  altShow (NST3 relfp) = altShow relfp


--  UNC = "\\" 1*pchar "\" 1*pchar  [ 1*"\" [ relative-path ] ]
data UNCShare = UNCShare Separator Separator
                         NonEmptyString
                         (NonEmpty Separator)
                         NonEmptyString
                         (Maybe (NonEmpty Separator, Maybe RelFilePath))
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[1] `AndShrinking` UNCShare)

instance AltShow UNCShare where
  altShow (UNCShare sep1 sep2 fp1 seps fp2 mrfp) = altShow sep1 ++ altShow sep2 ++ altShow fp1 ++ altShow seps ++ altShow fp2 ++ maybe "" (\(a, b) -> altShow a ++ maybe "" altShow b) mrfp

newtype NonEmptyString = NonEmptyString (NonEmpty Char)
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[1] `AndShrinking` NonEmptyString)

instance Semigroup NonEmptyString where
  (<>) (NonEmptyString ne) (NonEmptyString ne') = NonEmptyString (ne <> ne')

instance AltShow NonEmptyString where
  altShow (NonEmptyString ns) = NE.toList ns


-- | Windows API Namespaces
--
-- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#namespaces
-- https://support.microsoft.com/en-us/topic/70b92942-a643-2f2d-2ac6-aad8acad49fb
-- https://superuser.com/a/1096784/854039
-- https://reverseengineering.stackexchange.com/a/15178
-- https://stackoverflow.com/a/25099634
--
-- namespace          = file-namespace / device-namespace / nt-namespace
-- file-namespace     = "\" "\" "?" "\"
-- device-namespace   = "\" "\" "." "\"
-- nt-namespace       = "\" "?" "?" "\"
data NameSpace = FileNameSpace
               | DeviceNameSpace
               | NTNameSpace
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[3, 1, 1] `AndShrinking` NameSpace)

instance AltShow NameSpace where
  altShow FileNameSpace = "\\\\?\\"
  altShow DeviceNameSpace = "\\\\.\\"
  altShow NTNameSpace = "\\??\\"


data Separator = UnixSep
               | WindowsSep
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[1, 1] `AndShrinking` Separator)

instance AltShow Separator where
  altShow UnixSep = "/"
  altShow WindowsSep = "\\"

instance {-# OVERLAPS #-} AltShow (NonEmpty Separator) where
  altShow ne = mconcat $ NE.toList (altShow <$> ne)

instance {-# OVERLAPS #-} AltShow [Separator] where
  altShow [] = ""
  altShow ne = altShow (NE.fromList ne)

--  relative-path = 1*(path-name 1*"\") [ file-name ] / file-name
data RelFilePath = Rel1 (NonEmpty (NonEmptyString, NonEmpty Separator)) (Maybe FileName)
                 | Rel2 FileName
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[2, 1] `AndShrinking` RelFilePath)

instance AltShow RelFilePath where
  altShow (Rel1 ns mf) = mconcat (NE.toList $ fmap (\(a, b) -> altShow a ++ altShow b) ns) ++ altShow mf
  altShow (Rel2 fn) = altShow fn

--  file-name = 1*pchar [ stream ]
data FileName = FileName NonEmptyString (Maybe DataStream)
  deriving (GShow, Show, Eq, Ord, Generic)

instance Arbitrary FileName where
  -- make sure that half of the filenames include a dot '.'
  -- so that we can deal with extensions
  arbitrary = do
    ns <- arbitrary
    ds <- arbitrary
    i <- chooseInt (0, 100)
    if i >= 50
    then do
           ns' <- arbitrary
           pure $ FileName (ns <> NonEmptyString ('.':|[]) <> ns') ds
    else pure $ FileName ns ds
  shrink = genericShrink


instance Arbitrary (Maybe DataStream) where
  arbitrary = genericArbitraryRec (1 % 1 % ())
  shrink = genericShrink

instance AltShow FileName where
  altShow (FileName ns ds) = altShow ns ++ altShow ds

--  stream = ":" 1*schar [ ":" 1*schar ] / ":" ":" 1*schar
data DataStream = DS1 NonEmptyString (Maybe NonEmptyString)
                | DS2 NonEmptyString -- ::datatype
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryRec '[1, 1] `AndShrinking` DataStream)

instance AltShow DataStream where
  altShow (DS1 ns Nothing) = ":" ++ altShow ns
  altShow (DS1 ns (Just ns2)) = ":" ++ altShow ns ++ ":" ++ altShow ns2
  altShow (DS2 ns) = "::" ++ altShow ns

instance Arbitrary WindowsFilePaths where
  arbitrary = WindowsFilePaths <$> listOf' arbitrary
  shrink = genericShrink

instance Arbitrary [Separator] where
  arbitrary = listOf' arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> listOf1' arbitrary
  shrink = genericShrink

