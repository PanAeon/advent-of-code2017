module Day11 where


import           System.IO
import           System.IO.Unsafe
import qualified Data.Bits as B
import qualified Numeric as N
import Data.List.Split
import           Data.Char

task11Input = head $ unsafePerformIO $ do
                handle <- openFile "input/day11" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

stringCoords = splitOn "," task11Input

toCoord :: String -> (Int, Int, Int)
toCoord "n" = (0,  1, negate 1)
toCoord "s" = (0, negate 1,  1)
toCoord "ne" = (1, 0, negate 1)
toCoord "nw" = (negate 1, 1, 0)
toCoord "se" = ( 1, negate 1, 0)
toCoord "sw" = (negate 1, 0, 1)

moves = toCoord <$> stringCoords


distance :: [(Int, Int, Int)] -> Int
distance xs =  (abs cx + abs cy + abs cz) `div` 2
  where
    (cx, cy, cz) = foldr f (0,0,0) xs
    f (x0, y0, z0) (x, y, z) = (x0 +x, y0 + y, z0 + z)

allDistances :: [(Int, Int, Int)] -> Int
allDistances xs =  maximum $ reverse ds
  where
    rs = scanr f (0,0,0) xs
    ds = g <$> rs
    g (cx,cy,cz) = (abs cx + abs cy + abs cz) `div` 2
    f (x0, y0, z0) (x, y, z) = (x0 +x, y0 + y, z0 + z)

-- 720
-- 1132 <- to low (how this is possible?)
