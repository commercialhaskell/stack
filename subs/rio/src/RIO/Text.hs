module RIO.Text
  ( module X
  ) where

import Data.Text as X -- FIXME hide partials
import Data.Text.Encoding as X (encodeUtf8, decodeUtf8With, decodeUtf8')
import Data.Text.Encoding.Error as X (lenientDecode)
