{-# LANGUAGE QuasiQuotes #-}
module Day2Test where

import           Day2
import           Test.Tasty.Hspec
import           Text.RawString.QQ

divisibleTestCase = [r|
5 9 2 8
9 4 7 3
3 8 6 5
|]

checksumTestCase = [r|
5 1 9 5
7 5 3
2 4 6 8
|]

spec_day2 :: Spec
spec_day2 = do
  describe "computeDivisibleChecksum" $ do
    it "shoud compute test case 1" $ do
      computeDivisibleChecksum (parse divisibleTestCase) `shouldBe` ( 9 :: Int)
  describe "computeChecksum" $ do
    it "should compute test case 1" $ do
      computeChecksum (parse checksumTestCase) `shouldBe` (18 :: Int)
