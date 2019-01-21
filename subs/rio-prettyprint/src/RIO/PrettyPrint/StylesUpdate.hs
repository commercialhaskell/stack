{-# LANGUAGE NoImplicitPrelude          #-}

module RIO.PrettyPrint.StylesUpdate
  (
    StylesUpdate (..)
  , parseStylesUpdateFromString
  , HasStylesUpdate (..)
  ) where

import Data.Aeson (FromJSON(..), withText)
import Data.Array.IArray (assocs)
import Data.Colour.SRGB (Colour, sRGB24)
import Data.Text as T (pack, unpack)
import RIO
import RIO.PrettyPrint.DefaultStyles (defaultStyles)
import RIO.PrettyPrint.Types (Style, StyleSpec)
import System.Console.ANSI.Types (BlinkSpeed (..), Color (..),
         ColorIntensity (..), ConsoleIntensity (..), ConsoleLayer (..),
         SGR (..), Underlining (..))

-- |Updates to 'Styles'
newtype StylesUpdate = StylesUpdate { stylesUpdate :: [(Style, StyleSpec)] }
  deriving (Eq, Show)

-- |The first styles update overrides the second one.
instance Semigroup StylesUpdate where
  -- See module "Data.IArray.Array" of package @array@: this depends on GHC's
  -- implementation of '(//)' being such that the last value specified for a
  -- duplicated index is used.
  StylesUpdate s1 <> StylesUpdate s2 = StylesUpdate (s2 <> s1)

instance Monoid StylesUpdate where
  mempty = StylesUpdate []
  mappend = (<>) -- This needs to be specified as, before package
                 -- @base-4.11.0.0@ (GHC 8.4.2, March 2018), the default is
                 -- 'mappend = (++)'.

instance FromJSON StylesUpdate where
  parseJSON = withText "StylesUpdate" $
    return . parseStylesUpdateFromString . T.unpack

-- |Parse a string that is a colon-delimited sequence of key=value, where 'key'
-- is a style name and 'value' is a semicolon-delimited list of 'ANSI' SGR
-- (Select Graphic Rendition) control codes (in decimal). Keys that are not
-- present in 'defaultStyles' are ignored. Items in the semicolon-delimited
-- list that are not recognised as valid control codes are ignored.
parseStylesUpdateFromString :: String -> StylesUpdate
parseStylesUpdateFromString s = StylesUpdate $ mapMaybe process table
 where
  table = do
    w <- split ':' s
    let (k, v') = break (== '=') w
    case v' of
      '=' : v -> return (T.pack k, parseCodes v)
      _ -> []

  process :: StyleSpec -> Maybe (Style, StyleSpec)
  process (k, sgrs) = do
    style <- lookup k styles
    return (style, (k, sgrs))

styles :: [(Text, Style)]
styles = map (\(s, (k, _)) -> (k, s)) $ assocs defaultStyles

parseCodes :: String -> [SGR]
parseCodes [] = []
parseCodes s = parseCodes' c
 where
  s' = split ';' s
  c :: [Word8]
  c = mapMaybe readMaybe s'

parseCodes' :: [Word8] -> [SGR]
parseCodes' c = case codeToSGR c of
  (Nothing, []) -> []
  (Just sgr, []) -> [sgr]
  (Nothing, cs) -> parseCodes' cs
  (Just sgr, cs) -> sgr : parseCodes' cs

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest1 -> chunk : split c rest1
  where
    (chunk, rest) = break (==c) s

-- |This function is, essentially, the inverse of 'sgrToCode' exported by
-- module "System.Console.ANSI.Codes" of the @ansi-terminal@ package. The
-- \'ANSI\' standards refer to (1) standard ECMA-48 \`Control Functions for
-- Coded Character Sets\' (5th edition, 1991); (2) extensions in ITU-T
-- Recommendation (previously CCITT Recommendation) T.416 (03/93) \'Information
-- Technology – Open Document Architecture (ODA) and Interchange Format:
-- Character Content Architectures\` (also published as ISO/IEC International
-- Standard 8613-6); and (3) further extensions used by \'XTerm\', a terminal
-- emulator for the X Window System. The escape codes are described in a
-- Wikipedia article at <http://en.wikipedia.org/wiki/ANSI_escape_code> and
-- those codes supported on current versions of Windows at
-- <https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences>.
codeToSGR :: [Word8] -> (Maybe SGR, [Word8])
codeToSGR [] = (Nothing, [])
codeToSGR (c:cs)
  | c ==  0 = (Just Reset, cs)
  | c ==  1 = (Just $ SetConsoleIntensity BoldIntensity, cs)
  | c ==  2 = (Just $ SetConsoleIntensity FaintIntensity, cs)
  | c ==  3 = (Just $ SetItalicized True, cs)
  | c ==  4 = (Just $ SetUnderlining SingleUnderline, cs)
  | c ==  5 = (Just $ SetBlinkSpeed SlowBlink, cs)
  | c ==  6 = (Just $ SetBlinkSpeed RapidBlink, cs)
  | c ==  7 = (Just $ SetSwapForegroundBackground True, cs)
  | c ==  8 = (Just $ SetVisible False, cs)
  | c == 21 = (Just $ SetUnderlining DoubleUnderline, cs)
  | c == 22 = (Just $ SetConsoleIntensity NormalIntensity, cs)
  | c == 23 = (Just $ SetItalicized False, cs)
  | c == 24 = (Just $ SetUnderlining NoUnderline, cs)
  | c == 25 = (Just $ SetBlinkSpeed NoBlink, cs)
  | c == 27 = (Just $ SetSwapForegroundBackground False, cs)
  | c == 28 = (Just $ SetVisible True, cs)
  | c >= 30 && c <= 37 =
    (Just $ SetColor Foreground Dull $ codeToColor (c - 30), cs)
  | c == 38 = case codeToRGB cs of
    (Nothing, cs') -> (Nothing, cs')
    (Just color, cs') -> (Just $ SetRGBColor Foreground color, cs')
  | c >= 40 && c <= 47 =
    (Just $ SetColor Background Dull $ codeToColor (c - 40), cs)
  | c == 48 = case codeToRGB cs of
    (Nothing, cs') -> (Nothing, cs')
    (Just color, cs') -> (Just $ SetRGBColor Background color, cs')
  | c >= 90 && c <= 97 =
    (Just $ SetColor Foreground Vivid $ codeToColor (c - 90), cs)
  | c >= 100 && c <= 107 =
    (Just $ SetColor Background Vivid $ codeToColor (c - 100), cs)
  | otherwise = (Nothing, cs)

-- |This function is, essentially, the inverse of 'colorToCode' exported by
-- module "System.Console.ANSI.Codes" of the @ansi-terminal@ package. The
-- \'ANSI\' standards refer to eight named colours in a specific order. The code
-- is a 0-based index of those colours.
codeToColor :: Word8 -> Color
codeToColor c
  -- 'toEnum' is not used because the @ansi-terminal@ package does not
  -- /guarantee/ the order of the data constructors of type 'Color' will be the
  -- same as that of the \'ANSI\' standards (although it currently is). (The
  -- 'colorToCode' function itself does not use 'fromEnum'.)
  | c == 0 = Black
  | c == 1 = Red
  | c == 2 = Green
  | c == 3 = Yellow
  | c == 4 = Blue
  | c == 5 = Magenta
  | c == 6 = Cyan
  | c == 7 = White
  | otherwise = error "Error: codeToColor, code outside 0 to 7."

codeToRGB :: [Word8] -> (Maybe (Colour Float), [Word8])
codeToRGB [] = (Nothing, [])
codeToRGB (2:r:g:b:cs) = (Just $ sRGB24 r g b, cs)
codeToRGB cs = (Nothing, cs)

-- | Environment values with a styles update.
--
-- @since 0.1.0.0
class HasStylesUpdate env where
  stylesUpdateL :: Lens' env StylesUpdate
instance HasStylesUpdate StylesUpdate where
  stylesUpdateL = id
