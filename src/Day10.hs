module Day10(generateHash) where

import           System.IO
import           System.IO.Unsafe
import qualified Data.Bits as B
import qualified Numeric as N

import           Data.Char

task10Input = head $ unsafePerformIO $ do
                handle <- openFile "input/day10" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

suffix = [17, 31, 73, 47, 23]

grouped :: Int -> [a] -> [[a]]
grouped n [] = []
grouped n xs = (take n xs) : (grouped n (drop n xs))

generateHash :: String -> String
generateHash s = zs >>= toHex
  where
    bytes = ord <$> s
    bytes' = bytes ++ suffix
    -- FIXME: replace with iterate
    transformed = snd $ foldl (const .(tieAllKnots bytes')) ((0,0), [0..n-1])  [1..64]
    zs =  (foldr1 (B.xor)) <$> grouped 16 transformed

toHex :: Int -> String
toHex x = pad z1
  where
    pad x = if length x < 2 then '0':x else x
    z1 =  N.showHex x ""

input :: [Int]
input = [34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167]

test1 :: [Int]
test1 = [3, 4, 1, 5]

n = 256 :: Int
-- tieAllKnots :: [Int] -> [[Int]]
tieAllKnots' xs  = snd $ tieAllKnots xs ((0, 0), (take n [0..]) )
tieAllKnots :: [Int] -> ((Int, Int), [Int]) -> ((Int, Int),[Int])
tieAllKnots xs z0 = foldl f z0 xs
  where
    f ((skip, curr), as) l = ((skip+1, (l + skip + curr) `mod` n), rotate as curr l)
rotate xs p l = ws2 -- as ++ bs' ++ rs --take n $ drop (n - p ) $ cycle  ( (reverse xss) ++ yss)
      where
        cs = drop p (cycle xs) -- shift left by p
        rs1 = reverse (take l cs) -- reverse next l
        rs2 = drop l cs -- rest
        ws  = take n $ rs1 ++ rs2
        ws2 =  take n $ drop (n - p) $ cycle ws
        -- cs = cycle xs
        -- as = take p xs
        -- bs = take l $ drop p cs
        -- bs' = reverse bs
        -- rs  = take (n-l - (length as)) $ drop (l+p) cs
      --  xss = take l $ drop p $ cycle xs
        --yss = take (n - l) $ drop l $ drop p $ cycle xs


-- 37442 too low--
--54675
