module Day6Test where

import qualified Data.Vector.Unboxed as UV
import           Day6
import           Test.Tasty.Hspec


redistribute' :: [Int] -> Int -> [Int]
redistribute' xs n = UV.toList $ redistribute  (UV.fromList xs) n

spec_day6 :: Spec
spec_day6 = do
  describe "redistribute" $ do
    it "should process [0,2,7,0]" $ do
      redistribute' [0,2,7,0] 2 `shouldBe` [2,4,1,2]
    it "should process [2,4,1,2]" $ do
      redistribute' [2,4,1,2] 1 `shouldBe` [3,1,2,3]
    it "should process [3,1,2,3]" $ do
      redistribute' [3,1,2,3] 0 `shouldBe` [0,2,3,4]
    it "should process [0,2,3,4]" $ do
      redistribute' [0,2,3,4] 3 `shouldBe` [1,3,4,1]
    it "should process [1,3,4,1]" $ do
      redistribute' [1,3,4,1] 2 `shouldBe` [2,4,1,2]

-- TODO: it should save # of elements for all lists
