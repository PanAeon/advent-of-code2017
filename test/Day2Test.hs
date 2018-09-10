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

spec_day2 :: Spec
spec_day2 = do
  describe "computeDivisibleChecksum" $ do
    it "shoud compute test case 1" $ do
      computeDivisibleChecksum (parse divisibleTestCase) `shouldBe` ( 9 :: Int)
  -- describe "checksumShiftHalf" $ do
  --   it "handles 1212" $ do
  --     checksumShiftHalf "1212" `shouldBe` (6 :: Int)
  --   it "handles 1221" $ do
  --     checksumShiftHalf "1221" `shouldBe` (0 :: Int)
  --   it "handles 123425" $ do
  --     checksumShiftHalf "123425" `shouldBe` (4 :: Int)
  --   it "handles 123123" $ do
  --     checksumShiftHalf "123123" `shouldBe` (12 :: Int)
  --   it "handles 12131415" $ do
  --     checksumShiftHalf "12131415" `shouldBe` (4 :: Int)
