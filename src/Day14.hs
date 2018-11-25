module Day14
    (tx
    ) where

import Day10
import qualified Data.Bits as B
import qualified Numeric as N
import Data.Map(Map)
import qualified Data.Map as M
import Data.List

import           Data.Char
import Day12

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

--numRegions, it's the same as day 12, almost...

gridToGraph :: [Integer] -> [Node]
gridToGraph xs = (xs' >>= f)
  where
    prev = 0:xs
    next = (tail xs) ++ [0]
    xs' = zip4 prev xs next [0..]
    foo b v = if b then [v] else []
    idx i j = i + 128*j
    f (p, c, n, j) = ys
      where
        ys = [ (Node (idx i j) cs) | i <- [0..127],
                                       B.testBit c i,
                 let
                   cl = i > 0 && B.testBit c (i-1)
                   cr = i < 127 && B.testBit c (i+1)
                   ct = B.testBit p i
                   cb = B.testBit n i
                   cs = foo cl (idx (i-1) j) ++
                        foo cr (idx (i+1) j) ++
                        foo ct (idx (i) (j-1)) ++
                        foo cb (idx (i) (j+1))
             ]

-- 1242 for test
tx = gridToGraph (generateGrid "stpzcrnm")



-- now all adjacent, not including diagonals
-- Now, all the defragmenter needs to know is the number of regions.
