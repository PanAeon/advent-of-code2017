module Day14
    (
    ) where

import Day10
import qualified Data.Bits as B
import qualified Numeric as N

import           Data.Char

day14Input = "stpzcrnm"

testInput = "flqrgnkx"


generateGrid :: String -> [Integer]
generateGrid s = (fst . head  ) <$> N.readHex <$> hs
  where
    rs = [ s ++ "-" ++ (show i)| i <- [0..127]]
    hs = generateHash <$> rs

-- aha, doesn't pad?
showBits x = (replicate k '0') ++ s
  where
    s = N.showIntAtBase (2::Integer) (chr . (48+)) (x) ""
    k = 128 - length s

numUsed :: String -> Int
numUsed s = sum xs
  where
    g = generateGrid s
    xs = B.popCount <$> g

--numRegions, it's the same as previous, not ?

-- now all adjacent, not including diagonals
-- Now, all the defragmenter needs to know is the number of regions.
