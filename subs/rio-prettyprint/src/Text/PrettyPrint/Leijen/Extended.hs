{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module re-exports some of the interface for
-- "Text.PrettyPrint.Annotated.Leijen" along with additional definitions
-- useful for stack.
--
-- It defines a 'Monoid' instance for 'Doc'.
module Text.PrettyPrint.Leijen.Extended
  (
  -- * Pretty-print typeclass
  Pretty (..),

  -- * Ansi terminal Doc
  --
  -- See "System.Console.ANSI" for 'SGR' values to use beyond the colors
  -- provided.
  StyleDoc, StyleAnn(..),
  -- hDisplayAnsi,
  displayAnsi, displayPlain, renderDefault,

  -- * Selective re-exports from "Text.PrettyPrint.Annotated.Leijen"
  --
  -- Documentation of omissions up-to-date with @annotated-wl-pprint-0.7.0@

  -- ** Documents, parametrized by their annotations
  --
  -- Omitted compared to original: @putDoc, hPutDoc@
  -- Doc,

  -- ** Basic combinators
  --
  -- Omitted compared to original: @empty, char, text, (<>)@
  --
  -- Instead of @text@ and @char@, use 'fromString'.
  --
  -- Instead of @empty@, use 'mempty'.
  nest, line, linebreak, group, softline, softbreak,

  -- ** Alignment
  --
  -- The combinators in this section can not be described by Wadler's
  -- original combinators. They align their output relative to the
  -- current output position - in contrast to @nest@ which always
  -- aligns to the current nesting level. This deprives these
  -- combinators from being \`optimal\'. In practice however they
  -- prove to be very useful. The combinators in this section should
  -- be used with care, since they are more expensive than the other
  -- combinators. For example, @align@ shouldn't be used to pretty
  -- print all top-level declarations of a language, but using @hang@
  -- for let expressions is fine.
  --
  -- Omitted compared to original: @list, tupled, semiBraces@
  align, hang, indent, encloseSep,

  -- ** Operators
  --
  -- Omitted compared to original: @(<$>), (</>), (<$$>), (<//>)@
  (<+>),

  -- ** List combinators
  hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate,

  -- ** Fillers
  fill, fillBreak,

  -- ** Bracketing combinators
  enclose, squotes, dquotes, parens, angles, braces, brackets,

  -- ** Character documents
  -- Entirely omitted:
  --
  -- @
  -- lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  -- squote, dquote, semi, colon, comma, space, dot, backslash, equals,
  -- pipe
  -- @

  -- ** Primitive type documents
  -- Entirely omitted:
  --
  -- @
  -- string, int, integer, float, double, rational, bool,
  -- @

  -- ** Semantic annotations
  annotate, noAnnotate, styleAnn

  -- ** Rendering
  -- Original entirely omitted:
  -- @
  -- SimpleDoc(..), renderPretty, renderCompact, displayDecorated, displayDecoratedA, display, displayS, displayIO,
  -- SpanList(..), displaySpans
  -- @

  -- ** Undocumented
  -- Entirely omitted:
  -- @
  -- column, nesting, width
  -- @
  ) where

import Control.Monad.Reader (runReader, local)
import Data.Array.IArray ((!), (//))
import qualified Data.Text as T
import Distribution.ModuleName (ModuleName)
import qualified Distribution.Text (display)
import Path
import RIO
import qualified RIO.Map as M
import RIO.PrettyPrint.DefaultStyles (defaultStyles)
import RIO.PrettyPrint.Types (Style (Dir, File), Styles)
import RIO.PrettyPrint.StylesUpdate (StylesUpdate (..), HasStylesUpdate, stylesUpdateL)
import System.Console.ANSI (ConsoleLayer (..), SGR (..), setSGRCode)
import qualified Text.PrettyPrint.Annotated.Leijen as P
import Text.PrettyPrint.Annotated.Leijen
  ( Doc, SimpleDoc (..)
  )

-- TODO: consider smashing together the code for wl-annotated-pprint and
-- wl-pprint-text. The code here already handles doing the
-- ansi-wl-pprint stuff (better!) atop wl-annotated-pprint. So the
-- result would be a package unifying 3 different wl inspired packages.
--
-- Perhaps it can still have native string support, by adding a type
-- parameter to Doc?

instance Semigroup StyleDoc where
    StyleDoc x <> StyleDoc y = StyleDoc (x P.<> y)
instance Monoid StyleDoc where
    mappend = (<>)
    mempty = StyleDoc P.empty

--------------------------------------------------------------------------------
-- Pretty-Print class

class Pretty a where
    pretty :: a -> StyleDoc
    default pretty :: Show a => a -> StyleDoc
    pretty = StyleDoc . fromString . show

instance Pretty StyleDoc where
    pretty = id

instance Pretty (Path b File) where
    pretty = styleAnn File . StyleDoc . fromString . toFilePath

instance Pretty (Path b Dir) where
    pretty = styleAnn Dir . StyleDoc . fromString . toFilePath

instance Pretty ModuleName where
    pretty = StyleDoc . fromString . Distribution.Text.display

--------------------------------------------------------------------------------
-- Style Doc

-- |A style annotation.
newtype StyleAnn = StyleAnn (Maybe Style)
    deriving (Eq, Show, Semigroup)

instance Monoid StyleAnn where
    mempty = StyleAnn Nothing
    mappend = (<>)

-- |A document annotated by a style
newtype StyleDoc = StyleDoc { unStyleDoc :: Doc StyleAnn }
  deriving IsString

-- |An ANSI code(s) annotation.
newtype AnsiAnn = AnsiAnn [SGR]
    deriving (Eq, Show, Semigroup, Monoid)

-- |Convert a 'SimpleDoc' annotated with 'StyleAnn' to one annotated with
-- 'AnsiAnn', by reference to a 'Styles'.
toAnsiDoc :: Styles -> SimpleDoc StyleAnn -> SimpleDoc AnsiAnn
toAnsiDoc styles = go
  where
    go SEmpty        = SEmpty
    go (SChar c d)   = SChar c (go d)
    go (SText l s d) = SText l s (go d)
    go (SLine i d)   = SLine i (go d)
    go (SAnnotStart (StyleAnn (Just s)) d) =
        SAnnotStart (AnsiAnn (snd $ styles ! s)) (go d)
    go (SAnnotStart (StyleAnn Nothing) d) = SAnnotStart (AnsiAnn []) (go d)
    go (SAnnotStop d) = SAnnotStop (go d)

displayPlain
    :: (Pretty a, HasLogFunc env, HasStylesUpdate env,
        MonadReader env m, HasCallStack)
    => Int -> a -> m Utf8Builder
displayPlain w =
    displayAnsiSimple . renderDefault w . fmap (const mempty) . unStyleDoc . pretty

-- TODO: tweak these settings more?
-- TODO: options for settings if this is released as a lib

renderDefault :: Int -> Doc a -> SimpleDoc a
renderDefault = P.renderPretty 1

displayAnsi
    :: (Pretty a, HasLogFunc env, HasStylesUpdate env,
        MonadReader env m, HasCallStack)
    => Int -> a -> m Utf8Builder
displayAnsi w = do
    displayAnsiSimple . renderDefault w . unStyleDoc . pretty

{- Not used --------------------------------------------------------------------

hDisplayAnsi
    :: (Display a, HasAnsiAnn (Ann a), MonadIO m)
    => Handle -> Int -> a -> m ()
hDisplayAnsi h w x = liftIO $ do
    useAnsi <- hSupportsANSI h
    T.hPutStr h $ if useAnsi then displayAnsi w x else displayPlain w x

-}

displayAnsiSimple
    :: (HasLogFunc env, HasStylesUpdate env, MonadReader env m, HasCallStack)
    => SimpleDoc StyleAnn -> m Utf8Builder
displayAnsiSimple doc = do
    update <- view stylesUpdateL
    let styles = defaultStyles // stylesUpdate update
        doc' = toAnsiDoc styles doc
    return $
        flip runReader mempty $ displayDecoratedWrap go doc'
  where
    go (AnsiAnn sgrs) inner = do
        old <- ask
        let sgrs' = mapMaybe (\sgr -> if sgr == Reset
                                        then Nothing
                                        else Just (getSGRTag sgr, sgr)) sgrs
            new = if Reset `elem` sgrs
                      then M.fromList sgrs'
                      else foldl' (\mp (tag, sgr) -> M.insert tag sgr mp) old sgrs'
        (extra, contents) <- local (const new) inner
        return (extra, transitionCodes old new <> contents <> transitionCodes new old)
    transitionCodes old new =
        case (null removals, null additions) of
            (True, True) -> mempty
            (True, False) -> fromString (setSGRCode additions)
            (False, _) -> fromString (setSGRCode (Reset : M.elems new))
      where
        (removals, additions) = partitionEithers $ M.elems $
            M.mergeWithKey
               (\_ o n -> if o == n then Nothing else Just (Right n))
               (fmap Left)
               (fmap Right)
               old
               new

displayDecoratedWrap
    :: forall a m. Monad m
    => (forall b. a -> m (b, Utf8Builder) -> m (b, Utf8Builder))
    -> SimpleDoc a
    -> m Utf8Builder
displayDecoratedWrap f doc = do
    (mafter, result) <- go doc
    case mafter of
      Just _ -> error "Invariant violated by input to displayDecoratedWrap: no matching SAnnotStart for SAnnotStop."
      Nothing -> return result
  where
    spaces n = display (T.replicate n " ")

    go :: SimpleDoc a -> m (Maybe (SimpleDoc a), Utf8Builder)
    go SEmpty = return (Nothing, mempty)
    go (SChar c x) = liftM (fmap (display c <>)) (go x)
    -- NOTE: Could actually use the length to guess at an initial
    -- allocation.  Better yet would be to just use Text in pprint..
    go (SText _l s x) = liftM (fmap (fromString s <>)) (go x)
    go (SLine n x) = liftM (fmap ((display '\n' <>) . (spaces n <>))) (go x)
    go (SAnnotStart ann x) = do
        (mafter, contents) <- f ann (go x)
        case mafter of
            Just after -> liftM (fmap (contents <>)) (go after)
            Nothing -> error "Invariant violated by input to displayDecoratedWrap: no matching SAnnotStop for SAnnotStart."
    go (SAnnotStop x) = return (Just x, mempty)

{- Not used --------------------------------------------------------------------

-- Foreground color combinators

black, red, green, yellow, blue, magenta, cyan, white,
    dullblack, dullred, dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,
    onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
    ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan, ondullwhite
    :: Doc AnsiAnn -> Doc AnsiAnn
(black, dullblack, onblack, ondullblack) = colorFunctions Black
(red, dullred, onred, ondullred) = colorFunctions Red
(green, dullgreen, ongreen, ondullgreen) = colorFunctions Green
(yellow, dullyellow, onyellow, ondullyellow) = colorFunctions Yellow
(blue, dullblue, onblue, ondullblue) = colorFunctions Blue
(magenta, dullmagenta, onmagenta, ondullmagenta) = colorFunctions Magenta
(cyan, dullcyan, oncyan, ondullcyan) = colorFunctions Cyan
(white, dullwhite, onwhite, ondullwhite) = colorFunctions White

type EndoAnsiDoc = Doc AnsiAnn -> Doc AnsiAnn

colorFunctions :: Color -> (EndoAnsiDoc, EndoAnsiDoc, EndoAnsiDoc, EndoAnsiDoc)
colorFunctions color =
    ( ansiAnn [SetColor Foreground Vivid color]
    , ansiAnn [SetColor Foreground Dull color]
    , ansiAnn [SetColor Background Vivid color]
    , ansiAnn [SetColor Background Dull color]
    )

-}

styleAnn :: Style -> StyleDoc -> StyleDoc
styleAnn s = StyleDoc . P.annotate (StyleAnn (Just s)) . unStyleDoc

{- Not used --------------------------------------------------------------------

-- Intensity combinators

bold, faint, normal :: Doc AnsiAnn -> Doc AnsiAnn
bold = ansiAnn [SetConsoleIntensity BoldIntensity]
faint = ansiAnn [SetConsoleIntensity FaintIntensity]
normal = ansiAnn [SetConsoleIntensity NormalIntensity]

-}

-- | Tags for each field of state in SGR (Select Graphics Rendition).
--
-- It's a bit of a hack that 'TagReset' is included.
data SGRTag
    = TagReset
    | TagConsoleIntensity
    | TagItalicized
    | TagUnderlining
    | TagBlinkSpeed
    | TagVisible
    | TagSwapForegroundBackground
    | TagColorForeground
    | TagColorBackground
    | TagRGBColor
    deriving (Eq, Ord)

getSGRTag :: SGR -> SGRTag
getSGRTag Reset{}                       = TagReset
getSGRTag SetConsoleIntensity{}         = TagConsoleIntensity
getSGRTag SetItalicized{}               = TagItalicized
getSGRTag SetUnderlining{}              = TagUnderlining
getSGRTag SetBlinkSpeed{}               = TagBlinkSpeed
getSGRTag SetVisible{}                  = TagVisible
getSGRTag SetSwapForegroundBackground{} = TagSwapForegroundBackground
getSGRTag (SetColor Foreground _ _)     = TagColorForeground
getSGRTag (SetColor Background _ _)     = TagColorBackground
getSGRTag SetRGBColor{}                 = TagRGBColor

(<+>) :: StyleDoc -> StyleDoc -> StyleDoc
StyleDoc x <+> StyleDoc y = StyleDoc (x P.<+> y)

align :: StyleDoc -> StyleDoc
align = StyleDoc . P.align . unStyleDoc

noAnnotate :: StyleDoc -> StyleDoc
noAnnotate = StyleDoc . P.noAnnotate . unStyleDoc

braces :: StyleDoc -> StyleDoc
braces = StyleDoc . P.braces . unStyleDoc

angles :: StyleDoc -> StyleDoc
angles = StyleDoc . P.angles . unStyleDoc

parens :: StyleDoc -> StyleDoc
parens = StyleDoc . P.parens . unStyleDoc

dquotes :: StyleDoc -> StyleDoc
dquotes = StyleDoc . P.dquotes . unStyleDoc

squotes :: StyleDoc -> StyleDoc
squotes = StyleDoc . P.squotes . unStyleDoc

brackets :: StyleDoc -> StyleDoc
brackets = StyleDoc . P.brackets . unStyleDoc

annotate :: StyleAnn -> StyleDoc -> StyleDoc
annotate a = StyleDoc . P.annotate a . unStyleDoc

nest :: Int -> StyleDoc -> StyleDoc
nest a = StyleDoc . P.nest a . unStyleDoc

line :: StyleDoc
line = StyleDoc P.line

linebreak :: StyleDoc
linebreak = StyleDoc P.linebreak

fill :: Int -> StyleDoc -> StyleDoc
fill a = StyleDoc . P.fill a . unStyleDoc

fillBreak :: Int -> StyleDoc -> StyleDoc
fillBreak a = StyleDoc . P.fillBreak a . unStyleDoc

enclose :: StyleDoc -> StyleDoc -> StyleDoc -> StyleDoc
enclose l r x = l <> x <> r

cat :: [StyleDoc] -> StyleDoc
cat = StyleDoc . P.cat . map unStyleDoc

punctuate :: StyleDoc -> [StyleDoc] -> [StyleDoc]
punctuate (StyleDoc x) = map StyleDoc . P.punctuate x . map unStyleDoc

fillCat :: [StyleDoc] -> StyleDoc
fillCat = StyleDoc . P.fillCat . map unStyleDoc

hcat :: [StyleDoc] -> StyleDoc
hcat = StyleDoc . P.hcat . map unStyleDoc

vcat :: [StyleDoc] -> StyleDoc
vcat = StyleDoc . P.vcat . map unStyleDoc

sep :: [StyleDoc] -> StyleDoc
sep = StyleDoc . P.sep . map unStyleDoc

vsep :: [StyleDoc] -> StyleDoc
vsep = StyleDoc . P.vsep . map unStyleDoc

hsep :: [StyleDoc] -> StyleDoc
hsep = StyleDoc . P.hsep . map unStyleDoc

fillSep :: [StyleDoc] -> StyleDoc
fillSep = StyleDoc . P.fillSep . map unStyleDoc

encloseSep :: StyleDoc -> StyleDoc -> StyleDoc -> [StyleDoc] -> StyleDoc
encloseSep (StyleDoc x) (StyleDoc y) (StyleDoc z) =
  StyleDoc . P.encloseSep x y z . map unStyleDoc

indent :: Int -> StyleDoc -> StyleDoc
indent a = StyleDoc . P.indent a . unStyleDoc

hang :: Int -> StyleDoc -> StyleDoc
hang a = StyleDoc . P.hang a . unStyleDoc

softbreak :: StyleDoc
softbreak = StyleDoc P.softbreak

softline :: StyleDoc
softline = StyleDoc P.softline

group :: StyleDoc -> StyleDoc
group = StyleDoc . P.group . unStyleDoc
