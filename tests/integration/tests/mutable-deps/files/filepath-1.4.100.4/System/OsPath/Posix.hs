{- HLINT ignore -}
{-# LANGUAGE CPP #-}

#undef  WINDOWS
#define POSIX
#define IS_WINDOWS False
#define FILEPATH_NAME PosixPath
#define OSSTRING_NAME PosixString
#define WORD_NAME PosixChar

#include "Common.hs"

-- | QuasiQuote a 'PosixPath'. This accepts Unicode characters
-- and encodes as UTF-8. Runs 'isValid' on the input.
pstr :: QuasiQuoter
pstr =
  QuasiQuoter
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF8 ErrorOnCodingFailure) s
      when (not $ isValid ps) $ fail ("filepath not valid: " ++ show ps)
      lift ps
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
