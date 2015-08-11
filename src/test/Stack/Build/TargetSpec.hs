{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.Build.TargetSpec (main, spec) where

import qualified Data.Text           as T
import           Stack.Build.Target
import           Stack.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseRawTarget" $ do
        let test s e = it s $ parseRawTarget (T.pack s) `shouldBe` e
        test "foobar" $ Just $ RTPackage $(mkPackageName "foobar")
        test "foobar-1.2.3" $ Just $ RTPackageIdentifier $ PackageIdentifier
            $(mkPackageName "foobar") $(mkVersion "1.2.3")
        test "./foobar" Nothing
        test "foobar/" Nothing
        test "/foobar" Nothing
        test ":some-exe" $ Just $ RTComponent "some-exe"
        test "foobar:some-exe" $ Just $ RTPackageComponent $(mkPackageName "foobar") $ UnresolvedComponent "some-exe"
        test "foobar:exe:some-exe" $ Just $ RTPackageComponent $(mkPackageName "foobar")
            $ ResolvedComponent $ CExe "some-exe"
