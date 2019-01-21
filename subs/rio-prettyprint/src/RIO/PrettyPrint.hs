{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module RIO.PrettyPrint
    (
      -- * Type classes for optionally colored terminal output
      HasTerm (..), HasStylesUpdate (..)
      -- * Pretty printing functions
    , displayPlain, displayWithColor
      -- * Logging based on pretty-print typeclass
    , prettyDebug, prettyInfo, prettyNote, prettyWarn, prettyError, prettyWarnNoIndent, prettyErrorNoIndent
    , prettyDebugL, prettyInfoL, prettyNoteL, prettyWarnL, prettyErrorL, prettyWarnNoIndentL, prettyErrorNoIndentL
    , prettyDebugS, prettyInfoS, prettyNoteS, prettyWarnS, prettyErrorS, prettyWarnNoIndentS, prettyErrorNoIndentS
      -- * Semantic styling functions
      -- | These are used rather than applying colors or other styling directly,
      -- to provide consistency.
    , style
    , displayMilliseconds
      -- * Formatting utils
    , bulletedList
    , spacedBulletedList
    , debugBracket
      -- * Re-exports from "Text.PrettyPrint.Leijen.Extended"
    , Pretty (..), StyleDoc, StyleAnn (..)
    , nest, line, linebreak, group, softline, softbreak
    , align, hang, indent, encloseSep
    , (<+>)
    , hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate
    , fill, fillBreak
    , enclose, squotes, dquotes, parens, angles, braces, brackets
    , indentAfterLabel, wordDocs, flow
      -- * Re-exports from "RIO.PrettyPrint.Types.PrettyPrint"
    , Style (..)
    ) where

import Data.List (intersperse)
import RIO
import RIO.PrettyPrint.StylesUpdate (HasStylesUpdate (..))
import RIO.PrettyPrint.Types (Style (..))
import Text.PrettyPrint.Leijen.Extended (Pretty (pretty),
                     StyleAnn (..), StyleDoc, (<+>), align,
                     angles, braces, brackets, cat,
                     displayAnsi, displayPlain, dquotes, enclose, encloseSep,
                     fill, fillBreak, fillCat, fillSep, group, hang, hcat, hsep,
                     indent, line, linebreak,
                     nest, parens, punctuate, sep, softbreak, softline, squotes,
                     styleAnn, vcat, vsep)

class (HasLogFunc env, HasStylesUpdate env) => HasTerm env where
  useColorL :: Lens' env Bool
  termWidthL :: Lens' env Int

displayWithColor
    :: (HasTerm env, Pretty a, MonadReader env m, HasCallStack)
    => a -> m Utf8Builder
displayWithColor x = do
    useAnsi <- view useColorL
    termWidth <- view termWidthL
    (if useAnsi then displayAnsi else displayPlain) termWidth x

-- TODO: switch to using implicit callstacks once 7.8 support is dropped

prettyWith :: (HasTerm env, HasCallStack, Pretty b,
               MonadReader env m, MonadIO m)
           => LogLevel -> (a -> b) -> a -> m ()
prettyWith level f = logGeneric "" level . RIO.display <=< displayWithColor . f

-- Note: I think keeping this section aligned helps spot errors, might be
-- worth keeping the alignment in place.

prettyDebugWith, prettyInfoWith, prettyNoteWith, prettyWarnWith, prettyErrorWith, prettyWarnNoIndentWith, prettyErrorNoIndentWith
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => (a -> StyleDoc) -> a -> m ()
prettyDebugWith = prettyWith LevelDebug
prettyInfoWith  = prettyWith LevelInfo
prettyNoteWith f  = prettyWith LevelInfo
                          ((line <>) . (style Good "Note:" <+>) .
                           indentAfterLabel . f)
prettyWarnWith f  = prettyWith LevelWarn
                          ((line <>) . (style Warning "Warning:" <+>) .
                           indentAfterLabel . f)
prettyErrorWith f = prettyWith LevelError
                          ((line <>) . (style Error   "Error:" <+>) .
                           indentAfterLabel . f)
prettyWarnNoIndentWith f  = prettyWith LevelWarn
                                  ((line <>) . (style Warning "Warning:" <+>) . f)
prettyErrorNoIndentWith f = prettyWith LevelError
                                  ((line <>) . (style Error   "Error:" <+>) . f)

prettyDebug, prettyInfo, prettyNote, prettyWarn, prettyError, prettyWarnNoIndent, prettyErrorNoIndent
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => StyleDoc -> m ()
prettyDebug         = prettyDebugWith         id
prettyInfo          = prettyInfoWith          id
prettyNote          = prettyNoteWith          id
prettyWarn          = prettyWarnWith          id
prettyError         = prettyErrorWith         id
prettyWarnNoIndent  = prettyWarnNoIndentWith  id
prettyErrorNoIndent = prettyErrorNoIndentWith id

prettyDebugL, prettyInfoL, prettyNoteL, prettyWarnL, prettyErrorL, prettyWarnNoIndentL, prettyErrorNoIndentL
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => [StyleDoc] -> m ()
prettyDebugL         = prettyDebugWith         fillSep
prettyInfoL          = prettyInfoWith          fillSep
prettyNoteL          = prettyNoteWith          fillSep
prettyWarnL          = prettyWarnWith          fillSep
prettyErrorL         = prettyErrorWith         fillSep
prettyWarnNoIndentL  = prettyWarnNoIndentWith  fillSep
prettyErrorNoIndentL = prettyErrorNoIndentWith fillSep

prettyDebugS, prettyInfoS, prettyNoteS, prettyWarnS, prettyErrorS, prettyWarnNoIndentS, prettyErrorNoIndentS
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => String -> m ()
prettyDebugS         = prettyDebugWith         flow
prettyInfoS          = prettyInfoWith          flow
prettyNoteS          = prettyNoteWith          flow
prettyWarnS          = prettyWarnWith          flow
prettyErrorS         = prettyErrorWith         flow
prettyWarnNoIndentS  = prettyWarnNoIndentWith  flow
prettyErrorNoIndentS = prettyErrorNoIndentWith flow

-- End of aligned section

-- | Use after a label and before the rest of what's being labelled for
--   consistent spacing/indenting/etc.
--
--   For example this is used after "Warning:" in warning messages.
indentAfterLabel :: StyleDoc -> StyleDoc
indentAfterLabel = align

-- | Make a 'Doc' from each word in a 'String'
wordDocs :: String -> [StyleDoc]
wordDocs = map fromString . words

-- | Wordwrap a 'String'
flow :: String -> StyleDoc
flow = fillSep . wordDocs

debugBracket :: (HasCallStack, HasTerm env, MonadReader env m,
                 MonadIO m, MonadUnliftIO m) => StyleDoc -> m a -> m a
debugBracket msg f = do
  let output = logDebug . RIO.display <=< displayWithColor
  output $ "Start: " <> msg
  start <- getMonotonicTime
  x <- f `catch` \ex -> do
      end <- getMonotonicTime
      let diff = end - start
      output $ "Finished with exception in" <+> displayMilliseconds diff <> ":" <+>
          msg <> line <>
          "Exception thrown: " <> fromString (show ex)
      throwIO (ex :: SomeException)
  end <- getMonotonicTime
  let diff = end - start
  output $ "Finished in" <+> displayMilliseconds diff <> ":" <+> msg
  return x

-- |Annotate a 'StyleDoc' with a 'Style'.
style :: Style -> StyleDoc -> StyleDoc
style = styleAnn

-- Display milliseconds.
displayMilliseconds :: Double -> StyleDoc
displayMilliseconds t = style Good $
    fromString (show (round (t * 1000) :: Int)) <> "ms"

-- | Display a bulleted list of 'StyleDoc'.
bulletedList :: [StyleDoc] -> StyleDoc
bulletedList = mconcat . intersperse line . map (("*" <+>) . align)

-- | Display a bulleted list of 'StyleDoc' with a blank line between
-- each.
spacedBulletedList :: [StyleDoc] -> StyleDoc
spacedBulletedList = mconcat . intersperse (line <> line) . map (("*" <+>) . align)
