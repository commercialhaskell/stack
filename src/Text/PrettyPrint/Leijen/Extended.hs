{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  Display(..),

  -- * Ansi terminal Doc
  --
  -- See "System.Console.ANSI" for 'SGR' values to use beyond the colors
  -- provided.
  AnsiDoc, AnsiAnn(..), HasAnsiAnn(..),
  hDisplayAnsi, displayAnsi, displayPlain, renderDefault,

  -- ** Color combinators
  black, red, green, yellow, blue, magenta, cyan, white,
  dullblack, dullred, dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,
  onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
  ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan, ondullwhite,

  -- ** Intensity combinators
  bold, faint, normal,

  -- * Selective re-exports from "Text.PrettyPrint.Annotated.Leijen"
  --
  -- Documentation of omissions up-to-date with @annotated-wl-pprint-0.7.0@

  -- ** Documents, parametrized by their annotations
  --
  -- Omitted compared to original: @putDoc, hPutDoc@
  Doc,

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
  annotate, noAnnotate,

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

import Control.Monad.Reader
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), ConsoleIntensity(..), SGR(..), setSGRCode, hSupportsANSI)
import System.IO (Handle)
import qualified Text.PrettyPrint.Annotated.Leijen as P
import Text.PrettyPrint.Annotated.Leijen hiding ((<>), display)

-- TODO: consider smashing together the code for wl-annotated-pprint and
-- wl-pprint-text. The code here already handles doing the
-- ansi-wl-pprint stuff (better!) atop wl-annotated-pprint. So the
-- result would be a package unifying 3 different wl inspired packages.
--
-- Perhaps it can still have native string support, by adding a type
-- parameter to Doc?

instance Monoid (Doc a) where
    mappend = (P.<>)
    mempty = empty

--------------------------------------------------------------------------------
-- Pretty-Print class

class Display a where
    type Ann a
    type Ann a = AnsiAnn
    display :: a -> Doc (Ann a)
    default display :: Show a => a -> Doc (Ann a)
    display = fromString . show

instance Display (Doc a) where
    type Ann (Doc a) = a
    display = id


--------------------------------------------------------------------------------
-- Ansi Doc

type AnsiDoc = Doc AnsiAnn

newtype AnsiAnn = AnsiAnn [SGR]
    deriving (Eq, Ord, Show, Monoid)

class HasAnsiAnn a where
    getAnsiAnn :: a -> AnsiAnn
    toAnsiDoc :: Doc a -> AnsiDoc
    toAnsiDoc = fmap getAnsiAnn

instance HasAnsiAnn AnsiAnn where
    getAnsiAnn = id
    toAnsiDoc = id

instance HasAnsiAnn () where
    getAnsiAnn _ = mempty

displayPlain :: Display a => a -> T.Text
displayPlain = LT.toStrict . displayAnsiSimple . renderDefault . fmap (const mempty) . display

-- TODO: tweak these settings more?
-- TODO: options for settings if this is released as a lib

renderDefault :: Doc a -> SimpleDoc a
renderDefault = renderPretty 1 120

displayAnsi :: (Display a, HasAnsiAnn (Ann a)) => a -> T.Text
displayAnsi = LT.toStrict . displayAnsiSimple . renderDefault . toAnsiDoc . display

hDisplayAnsi
    :: (Display a, HasAnsiAnn (Ann a), MonadIO m)
    => Handle -> a -> m ()
hDisplayAnsi h x = liftIO $ do
    useAnsi <- hSupportsANSI h
    T.hPutStr h $ if useAnsi then displayAnsi x else displayPlain x

displayAnsiSimple :: SimpleDoc AnsiAnn -> LT.Text
displayAnsiSimple doc =
     LTB.toLazyText $ flip runReader mempty $ displayDecoratedWrap go doc
  where
    go (AnsiAnn sgrs) inner = do
        old <- ask
        let sgrs' = mapMaybe (\sgr -> if sgr == Reset then Nothing else Just (getSGRTag sgr, sgr)) sgrs
            new = if Reset `elem` sgrs
                      then M.fromList sgrs'
                      else foldl (\mp (tag, sgr) -> M.insert tag sgr mp) old sgrs'
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
    => (forall b. a -> m (b, LTB.Builder) -> m (b, LTB.Builder))
    -> SimpleDoc a
    -> m LTB.Builder
displayDecoratedWrap f doc = do
    (mafter, result) <- go doc
    case mafter of
      Just _ -> fail "Invariant violated by input to displayDecoratedWrap: no matching SAnnotStart for SAnnotStop."
      Nothing -> return result
  where
    spaces n = LTB.fromText (T.replicate n " ")

    go :: SimpleDoc a -> m (Maybe (SimpleDoc a), LTB.Builder)
    go SEmpty = return (Nothing, mempty)
    go (SChar c x) = liftM (fmap (LTB.singleton c <>)) (go x)
    -- NOTE: Could actually use the length to guess at an initial
    -- allocation.  Better yet would be to just use Text in pprint..
    go (SText _l s x) = liftM (fmap (fromString s <>)) (go x)
    go (SLine n x) = liftM (fmap ((LTB.singleton '\n' <>) . (spaces n <>))) (go x)
    go (SAnnotStart ann x) = do
        (mafter, contents) <- f ann (go x)
        case mafter of
            Just after -> liftM (fmap (contents <>)) (go after)
            Nothing -> error "Invariant violated by input to displayDecoratedWrap: no matching SAnnotStop for SAnnotStart."
    go (SAnnotStop x) = return (Just x, mempty)

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

ansiAnn :: [SGR] -> Doc AnsiAnn -> Doc AnsiAnn
ansiAnn = annotate . AnsiAnn

-- Intensity combinators

bold, faint, normal :: Doc AnsiAnn -> Doc AnsiAnn
bold = ansiAnn [SetConsoleIntensity BoldIntensity]
faint = ansiAnn [SetConsoleIntensity FaintIntensity]
normal = ansiAnn [SetConsoleIntensity NormalIntensity]

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
