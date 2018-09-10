module Day1Test where

import           Day1
import           Test.Tasty.Hspec

spec_checksumShift1 :: Spec
spec_checksumShift1 = do
  describe "checksumShift1" $ do
    it "handles 1122" $ do
      checksumShift1 "1122" `shouldBe` (3 :: Int)
    it "handles 1111" $ do
      checksumShift1 "1111" `shouldBe` (4 :: Int)
    it "handles 1234" $ do
      checksumShift1 "1234" `shouldBe` (0 :: Int)
    it "handles 91212129" $ do
      checksumShift1 "91212129" `shouldBe` (9 :: Int)
  describe "checksumShiftHalf" $ do
    it "handles 1212" $ do
      checksumShiftHalf "1212" `shouldBe` (6 :: Int)
    it "handles 1221" $ do
      checksumShiftHalf "1221" `shouldBe` (0 :: Int)
    it "handles 123425" $ do
      checksumShiftHalf "123425" `shouldBe` (4 :: Int)
    it "handles 123123" $ do
      checksumShiftHalf "123123" `shouldBe` (12 :: Int)
    it "handles 12131415" $ do
      checksumShiftHalf "12131415" `shouldBe` (4 :: Int)
