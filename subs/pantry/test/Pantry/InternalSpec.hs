module Pantry.InternalSpec (spec) where

import Test.Hspec
import Pantry.Internal (normalizeParents)

spec :: Spec
spec = describe "normalizeParents" $ do
  let (!) :: HasCallStack => String -> Maybe String -> Spec
      input ! output =
        it input $
          let x = normalizeParents input
              y = either (const Nothing) Just x
           in y `shouldBe` output

  "/file/\\test" ! Nothing
  "file/\\test" ! Just "file/\\test"
  "/file/////\\test" ! Nothing
  "file/////\\test" ! Just "file/\\test"
  "/file/\\test////" ! Nothing
  "/file/./test" ! Nothing
  "file/./test" ! Just "file/test"
  "/test/file/../bob/fred/" ! Nothing
  "/test/file/../bob/fred" ! Nothing
  "test/file/../bob/fred/" ! Nothing
  "test/file/../bob/fred" ! Just "test/bob/fred"
  "../bob/fred/" ! Nothing
  "./bob/fred/" ! Nothing
  "./bob/fred" ! Just "bob/fred"
  "./" ! Nothing
  "./." ! Nothing
  "/./" ! Nothing
  "/" ! Nothing
  "bob/fred/." ! Nothing
  "//home" ! Nothing
  "foobarbaz\\bin" ! Just "foobarbaz\\bin"
