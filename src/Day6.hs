{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Day6
    (redistribute,
     cycleLength,
     input6
    ) where

import qualified Data.List           as L
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Vector         as V
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import           Debug.Trace

input6 :: Vector Int
input6 = UV.fromList [0,5,10, 0,11,14,13,4,11,8,8,7,1,4,12,11]

findMostOccuppied :: Vector Int -> Int
findMostOccuppied = UV.maxIndex

redistribute :: Vector Int -> Int -> Vector Int
redistribute vs j = uncurry (+) `UV.map` UV.zip vs' zs'
  where
    n  = UV.length vs
    i  = UV.unsafeIndex vs j
    xs = replicate n (i `div` n) -- number of whole inclusions
    m  = i `mod` n
    ys = replicate m 1 ++ (replicate (n-m) 0)
    zs = uncurry (+) <$> zip xs ys
    zs' = UV.fromList $ take n $ drop (n - j - 1) $ cycle zs
    vs' = UV.unsafeUpd vs [(j, 0)]


-- FIXME: rewrite this plain recursion
f !k !vs !m = {-traceShow vs' $ -} if S.member vs' m
                 then k
                 else f (k+1) vs' (S.insert vs' m)
  where
    i = UV.maxIndex vs
    vs' = redistribute vs i


-- shit, not previous, but all previous!!
numRedistributions :: Vector Int -> Int
numRedistributions vs = f 1 vs S.empty
  -- where
  --   z = foldr (a -> b -> b) b0 [2..]
  --   f k (l, vs, m) = (k, vs', m')
  --     where
  --       i = UV.maxIndex vs
  --       vs' = redistribute vs i
    -- f vs = let
    --
    --

cycleLength' :: Vector Int -> Vector Int
cycleLength' vs = h 1 vs S.empty

h !k !vs !m = {-traceShow vs' $ -} if S.member vs' m
                 then vs'
                 else h (k+1) vs' (S.insert vs' m)
  where
    i = UV.maxIndex vs
    vs' = redistribute vs i



-----------------------------------------------------
cycleLength :: Vector Int -> Int
cycleLength vs = g 1 vs (UV.fromList [10,9,8,7,6,5,4,3,1,1,0,15,14,13,11,12])





-- i*128 + j
genUpdates :: V.Vector (Vector Int)
genUpdates = id $! V.fromList $ do
              i <- [0..15]
              j <- [0..127]
              pure $ genUpdate i j

genUpdate :: Int -> Int -> Vector Int
genUpdate j i = zs''
  where

    n  = 16
    xs = replicate n (i `div` n) -- number of whole inclusions
    m  = i `mod` n
    ys = replicate m 1 ++ (replicate (n-m) 0)
    zs = uncurry (+) <$> zip xs ys
    zs' = UV.fromList $ take n $ drop (n - j - 1) $ cycle zs
    p = UV.unsafeIndex zs' j
    zs'' = UV.unsafeUpd zs' [(j, p - i)]
    -- vs' = UV.unsafeUpd vs [(j, 0)]
    -- foo = uncurry (+) `UV.map` UV.zip vs' zs'

medebug n vs = if n `mod` 10000000 == 0
            then traceShow vs
            else id


redistribute' vs i = uncurry (+) `UV.map` UV.zip vs zs
  where
    j = UV.unsafeIndex vs i
    zs = V.unsafeIndex genUpdates (i*128 + j)

g !k !vs !m = {-medebug k vs' $ -} if  vs' == m
                 then k
                 else g (k+1) vs' m
  where
    i = UV.maxIndex vs
    vs' = redistribute' vs i

{-
--- Part Two ---

Out of curiosity, the debugger would also like to know the size of the loop:
starting from a state that has already been seen,
how many block redistribution cycles must be performed before that same state is seen
again?

In the example above, 2 4 1 2 is seen again after four cycles,
and so the answer in that example would be 4.

How many cycles are in the infinite loop that arises from the configuration
in your puzzle input?

-}
