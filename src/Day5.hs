{-# LANGUAGE LambdaCase #-}
module Day5
    (iter, task5Input
    ) where

-- import           Data.List
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import           System.IO
import           System.IO.Unsafe

task5Input = unsafePerformIO $ do
                handle <- openFile "/Users/edevi86/Downloads/day5input" ReadMode
                contents <- hGetContents handle
                pure $ (read <$> lines contents :: [Int])

third (a,b,c) = c
second (a,b,c) = b




getUpdate :: Int -> (a -> a) -> [a] -> (a, [a])
getUpdate i f xs = (b, xs')
  where
    (as, b:bs) = (take i xs, drop i xs)
    xs' = as ++ (f b : bs)

-- iter :: [Int] -> Int
-- TODO: faster then list but zillion time slower then mutable java.. think ...
-- hmm, maybe optimization may help
iter xs = third $ head $ dropWhile ((< n) . second)   ys
  where
    n  = length xs
    as = UV.fromList xs
    ys = iterate f (as, 0, 0)
    f (as, p, i) = (as', p+j, i+1)
      where
        j = UV.unsafeIndex as p
        j' = if j >= 3 then j - 1 else j + 1
        as' = UV.unsafeUpd as [(p, j')]
    -- ys = foldl

-- 343467 wrong

-- part II
-- Now, the jumps are even stranger: after each jump, if the offset was three or more, instead decrease it by 1. Otherwise, increase it by 1 as before.
