{-# LANGUAGE NoImplicitPrelude #-}
{-|
For the most part, the data constructors of 'Style' do not clash with other
names. When they do, import the module qualified. For example:

> import qualified RIO.PrettyPrint.Types.PrettyPrint as PP
-}
module RIO.PrettyPrint.Types
  (
    Style (..)
  , Styles
  , StyleSpec
  ) where

import Data.Array.IArray (Array)
import Data.Ix (Ix)
import Data.Text (Text)
import RIO
import System.Console.ANSI.Types (SGR)

-- |A style of rio-prettyprint's output.
data Style
  = Error    -- Should be used sparingly, not to style entire long messages. For
             -- example, it's used to style the "Error:" label for an error
             -- message, not the entire message.
  | Warning  -- Should be used sparingly, not to style entire long messages. For
             -- example, it's used to style the "Warning:" label for an error
             -- message, not the entire message.
  | Good     -- Style in a way to emphasize that it is a particularly good
             -- thing
  | Shell    -- Style as a shell command, i.e. when suggesting something to the
             -- user that should be typed in directly as written.
  | File     -- Style as a filename. See 'Dir' for directories.
  | Url      -- Style as a URL.
  | Dir      -- Style as a directory name. See 'File' for files.
  | Recommendation  -- Style used to highlight part of a recommended course of
                    -- action.
  | Current  -- Style in a way that emphasizes that it is related to a current
             -- thing. For example, could be used when talking about the current
             -- package we're processing when outputting the name of it.
  | Target   -- TODO: figure out how to describe this
  | Module   -- Style as a module name
  | PkgComponent    -- Style used to highlight the named component of a package.
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

-- |The first style overrides the second.
instance Semigroup Style where
  s <> _ = s

-- |A style specification, pairing its \'key\' with the corresponding list of
-- 'SGR' codes.
type StyleSpec = (Text, [SGR])

-- |Style specifications indexed by the style.
type Styles = Array Style StyleSpec
