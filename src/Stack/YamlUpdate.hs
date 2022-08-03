{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Update YAML preserving top-level key order, blank lines and comments.
--
-- The call sequence is mkRaw, encodeInOrder, redress and unmkRaw but if you
-- don't care about preserving trailing blank lines this can be simplified to
-- encodeInOrder and redress.
--
-- Use yamlLines to transform 'RawYaml' to ['RawYamlLine'].
module Stack.YamlUpdate
    ( encodeInOrder, redress
    , mkRaw, unmkRaw
    , yamlLines
    , RawYaml(..), RawYamlLine(..), YamlKey(..)
    ) where

import           Stack.Prelude
import           Data.Coerce (coerce)
import qualified Data.List as L
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml
import qualified RIO.Text as T
import qualified RIO.Map as Map

-- | A whole YAML document, may contain line breaks.
newtype RawYaml = RawYaml Text deriving newtype Display
-- | One line from a YAML document, shouldn't contain line breaks.
newtype RawYamlLine = RawYamlLine Text
-- | A YAML top-level key as in @key: value@.
newtype YamlKey = YamlKey Text deriving newtype (Eq, Display)

-- | The line number of a blank line.
newtype YamlLineBlank = YamlLineBlank Int deriving newtype Display
-- | A line number and some content, usually a comment. This can be used with an
-- empty comment to carry the line number for a blank line.
newtype YamlLineComment = YamlLineComment (Int, Text)
-- | A mapping from the line number after an encoding that strips blank lines
-- and comments to a line number of the original document.
newtype YamlLineReindex = YamlLineReindex (Int, Int)

data YamlLines =
    YamlLines
        { blanks :: ![YamlLineBlank]
        -- ^ The line numbers of blank lines.
        , wholeLineComments :: ![YamlLineComment]
        -- ^ Comments where # is the first non-space character in that line so
        -- that the comment takes up the whole line. Captured with the leading
        -- spaces.
        , partLineComments :: ![YamlLineComment]
        -- ^ Comments that have been apended to a line.
        , reindices :: ![YamlLineReindex]
        -- ^ Bumps for line numbers that will need to be moved when blank lines
        -- and whole line comments are added back in.
        }

data Pegged =
    Pegged
        { newIndex :: !Int
        -- ^ The new line number to put a line of content.
        , leading :: ![YamlLineComment]
        -- ^ Comments for putting before anything else.
        , partComments :: ![YamlLineComment]
        -- ^ Comments to be appended to lines.
        , spanComments :: ![YamlLineComment]
        -- ^ Blank lines and whole line comments from a range to be put back on
        -- the same line as they were taken from.
        }

-- | Converts raw YAML as 'Text' with line breaks into a list of lines, dropping
-- trailing line breaks.
yamlLines :: RawYaml -> [RawYamlLine]
yamlLines x = RawYamlLine <$> T.lines (coerce x)

-- | Puts blank lines and comments from the original lines into the update.
redress :: [RawYamlLine] -> RawYaml -> RawYaml
redress rawLines (RawYaml t) = let xs = zip [1 ..] (T.lines t) in RawYaml . T.concat $
    [
        T.unlines . fromMaybe [x] $ do
            Pegged{newIndex = i', leading, partComments, spanComments} <- fetchPegged rawLines (i, j)

            let x' = maybe
                        x
                        (\(YamlLineComment (_, c)) -> x <> " " <> dropToComment c)
                        (L.find ((== i') . commentLineNumber) partComments)

            let cs = x' : (comment <$> spanComments)

            return $ if i /= 1 then cs else (comment <$> leading) ++ cs

    | (i, x) <- xs
    | (j, _) <- drop 1 xs ++ [(0, "")]
    ]

fetchPegged :: [RawYamlLine] -> (Int, Int) -> Maybe Pegged
fetchPegged (pegLines -> yl@YamlLines{reindices}) (i, j) = do
    let reindex = flip L.lookup (coerce reindices)

    i' <- reindex i
    j' <- reindex j

    let (ps, spanned) = fetchInRange yl (\b -> i' <= b && b < j')

    return $ Pegged
        { newIndex = i'
        , leading = if i /= 1 then [] else snd $ fetchInRange yl (\b -> b < i')
        , partComments = ps
        , spanComments = spanned
        }

fetchInRange :: YamlLines -> (Int -> Bool) -> ([YamlLineComment], [YamlLineComment])
fetchInRange YamlLines{blanks, wholeLineComments, partLineComments} p =
    let lineNumbers = filter p $ coerce blanks
        ls = (\line -> YamlLineComment (line, "")) <$> lineNumbers
        filterLineNumber = filter (p . commentLineNumber) 
        cs = filterLineNumber wholeLineComments
        ps = filterLineNumber partLineComments
    in
        (ps, L.sortOn commentLineNumber $ ls ++ cs)

-- | Uses the order of the keys in the original to preserve the order in the
-- update except that inserting a key orders it last.
encodeInOrder
    :: [RawYamlLine]
    -> [YamlKey]
    -> YamlKey
    -> Yaml.Object
    -> Either UnicodeException RawYaml
encodeInOrder rawLines keysFound upsertKey@(YamlKey k) yObject =
    let keyLine = findKeyLine rawLines
        ixMap = Map.fromList $ (\yk@(YamlKey x) -> (x, keyLine yk)) <$> keysFound
        preservingCompare x y =
            -- If updating then preserve order but if inserting then put last.
            if | upsertKey `L.elem` keysFound -> Map.lookup x ixMap `compare` Map.lookup y ixMap
               | k == x, k == y -> EQ
               | k == x -> GT
               | k == y -> LT
               | otherwise -> Map.lookup x ixMap `compare` Map.lookup y ixMap

        keyCmp = Yaml.setConfCompare preservingCompare Yaml.defConfig
    
    in RawYaml <$> decodeUtf8' (Yaml.encodePretty keyCmp yObject)

endSentinel :: Text
endSentinel = "ED10F56C-562E-4847-A50B-7541C1732A15: 2986F150-E4A0-41D8-AB9C-8BD82FA12DC4"

mkRaw :: Text -> RawYaml
mkRaw = addSentinels . RawYaml

unmkRaw :: RawYaml -> Text
unmkRaw = coerce . removeSentinels

-- | This is leaking implementation but adding a sentinal key-value to the end
-- of YAML is a cheap way to ensure trailing newlines are not swallowed.
addSentinels :: RawYaml -> RawYaml
addSentinels (RawYaml x) = RawYaml $ x <> endSentinel

removeSentinels :: RawYaml -> RawYaml
removeSentinels (RawYaml x) = RawYaml . T.unlines . filter (/= endSentinel) $ T.lines x

findKeyLine :: [RawYamlLine] -> YamlKey -> Maybe Int
findKeyLine rawLines (YamlKey x) = join . listToMaybe . take 1 . dropWhile isNothing $
    [ if x `T.isPrefixOf` y then Just i else Nothing
    | RawYamlLine y <- rawLines
    | i <- [1 ..]
    ]

comment :: YamlLineComment -> Text
comment (YamlLineComment (_, c)) = c

commentLineNumber :: YamlLineComment -> Int
commentLineNumber (YamlLineComment (c, _)) = c

instance Display YamlLineComment where
    textDisplay (YamlLineComment (i, s)) = textDisplay . T.pack $ show (i, T.unpack s)

dropToComment :: Text -> Text
dropToComment = T.dropWhile (/= '#') 

-- | Gather enough information about lines to peg line numbers so that blank
-- lines and comments can be reinserted later.
pegLines :: [RawYamlLine] -> YamlLines
pegLines rawLines =
    let (ls, rs) = partitionEithers
                    [
                        if | y == "" -> Left . Left $ YamlLineBlank i

                           | "#" `T.isPrefixOf` T.dropWhile (== ' ') y ->
                                Left . Right $ YamlLineComment (i, y)

                           | otherwise ->
                                if "#" `T.isPrefixOf` dropToComment y
                                    then Right . Left $ YamlLineComment (i, y)
                                    else Right $ Right i

                    | RawYamlLine y <- rawLines
                    | i <- [1 ..]
                    ]

        (blanks, wholeLineComments) = partitionEithers ls
        (partLineComments, contentLines) = partitionEithers rs
        indexLines = L.sort $ contentLines ++ (commentLineNumber <$> partLineComments)
        reindex = zipWith (curry YamlLineReindex) [1 ..] indexLines

    in YamlLines blanks wholeLineComments partLineComments reindex