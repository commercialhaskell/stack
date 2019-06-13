module Pantry.InternalSpec (spec) where

import Test.Hspec
import Pantry (runPantryApp)
import Pantry.Internal (normalizeParents, makeTarRelative, hpackVersion)

spec :: Spec
spec = do
  describe "normalizeParents" $ do
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
    "file/test/" ! Just "file/test"
    "/file/\\test////" ! Nothing
    "/file/./test" ! Nothing
    "file/./test" ! Just "file/test"
    "/test/file/../bob/fred/" ! Nothing
    "/test/file/../bob/fred" ! Nothing
    "test/file/../bob/fred/" ! Just "test/bob/fred"
    "test/file/../bob/fred" ! Just "test/bob/fred"
    "../bob/fred" ! Nothing
    "../bob/fred/" ! Nothing
    "./bob/fred/" ! Just "bob/fred"
    "./bob/fred" ! Just "bob/fred"
    "./" ! Nothing
    "./." ! Nothing
    "/./" ! Nothing
    "/" ! Nothing
    "bob/fred/." ! Nothing
    "//home" ! Nothing
    "foobarbaz\\bin" ! Just "foobarbaz\\bin"

  describe "makeTarRelative" $ do
    let test :: HasCallStack => FilePath -> FilePath -> Maybe FilePath -> Spec
        test base rel expected =
          it (show (base, rel)) $
          either (const Nothing) Just (makeTarRelative base rel)
          `shouldBe` expected

    test "foo/bar" "baz" $ Just "foo/baz"
    test "foo" "bar" $ Just "bar"
    test "foo" "/bar" Nothing
    test "foo/" "bar" Nothing

    -- MSS 2018-08-23: Arguable whether this should be Nothing
    -- instead, since we don't want any absolute paths. However,
    -- that's really a concern for normalizeParents. Point being: if
    -- you refactor in the future, and this turns into Nothing, that's
    -- fine.
    test "/foo" "bar" $ Just "/bar"

  describe "Parse HPack version" $ do
    {-
    let isVersion :: Version -> Bool
        isVersion _ = True
    -}

    it "Shipped hpack version" $ example $ do
      _version <- runPantryApp hpackVersion
      -- version `shouldSatisfy` isVersion
      pure ()

    -- it "External hpack version" $ do
    --   version <- runPantryApp $ customHpack "/home/sibi/.local/bin/hpack" hpackVersion
    --   version `shouldSatisfy` isVersion
