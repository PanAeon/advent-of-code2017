module Day15(runCompetition) where

import qualified Data.Bits as B
import qualified Numeric as N
import           Data.Char
--Generator A starts with 289
--Generator B starts with 629

{-
As they do this, a judge waits for each of them to generate its next value, compares the lowest 16 bits of both values,
 and keeps track of the number of times those parts of the values match.

The generators both work on the same principle. To create its next value, a generator will take the previous value it produced,
multiply it by a factor (generator A uses 16807; generator B uses 48271),
and then keep the remainder of dividing that resulting product by 2147483647.
That final remainder is the value it produces next.

To calculate each generator's first value, it instead uses a specific starting value as its "previous value" (as listed in your puzzle input).
--Gen. A--  --Gen. B--
   1092455   430625591
1181022009  1233683848
 245556042  1431495498
1744312007   137874439
1352636452   285222916

    Generator A looks for values that are multiples of 4.
    Generator B looks for values that are multiples of 8.

-}



generator :: Int -> Int -> Int
generator factor i = (i * factor) `mod` 2147483647

generatorA = generator 16807
generatorB = generator 48271

showBits x = (replicate k '0') ++ s
  where
    s = N.showIntAtBase (2::Int) (chr . (48+)) (x) ""
    k = 64 - length s

zb = (B.shift (1::Int) 16) - 1
trunc :: Int -> Int
trunc i = i B..&. zb

runCompetition = sum $ f <$> zs
  where
    as = filter (\x -> x `mod` 4 == 0) $ iterate generatorA 289
    bs = filter (\x -> x `mod` 8 == 0) $ iterate generatorB 629
    zs = take 5000000 (zip as bs)
    f (x,y) = if trunc x == trunc y then 1 else 0
