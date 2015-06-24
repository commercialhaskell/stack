module Stack.Build.ExecuteSpec (main, spec) where

import Stack.Build.Execute
import Test.Hspec
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "compareTestComponents" $ do
    let test comps names expected = it (show (comps, names)) $
            compareTestsComponents
                (T.words $ T.pack comps)
                (T.words $ T.pack names)
                `shouldBe`
                (T.words $ T.pack expected)

    test "" "" ""
    test "" "foo" "foo"
    test "foo" "bar" ""
    test "foo" "foo bar" "foo"
    test "test:foo" "foo bar" "foo"
    test "test:foo exe:bar" "foo bar" "foo"
