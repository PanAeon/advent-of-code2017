{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Day2x
    (testInput, input, bar
    ) where



import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
import           Data.List
import           Text.Parsec.Char            (alphaNum, anyChar, char, digit, oneOf, satisfy, spaces, string)
import           Text.Parsec.String          (Parser)
-- import           Text.Parsec.String.Char
import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust)
import           Data.Set                    (Set)
import qualified Data.Set                    as S
-- import qualified Data.Vector.Algorithms.Insertion as IS
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Numeric                     as N
import           Text.Parsec.Combinator      (between, many1, sepBy)
import           Text.Parsec.Prim            (parse, parseTest, try)
-- import GHC.Prim
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Ord
import           Data.Vector.Unboxed         (freeze)
import qualified Data.Vector.Unboxed         as VU
import           Debug.Trace
-- import  Control.Monad.Primitive(PrimMonad)
import           Data.Either

inputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

input = inputRaw  "input/day2x"

testInput = [
   "abcde",
   "fghij",
   "klmno",
   "pqrst",
   "fguij",
   "axcye",
   "wvxyz"

   ]

foo = a * b
  where
    a = length $ filter twoOf input
    b = length $ filter threeOf input

----------------------------------------

bar xs = res
  where
    res = filter (\ys -> length ys >0) [ diffOne x y | x <- xs, y <- xs, x /= y]
    diffOne x y = [ (x) | (a,x) <- xs, (b,y) <- ys, x == y]
      where
        xs = fmap boo $  tail $ init $ zip (inits x) (tails x)
        ys = fmap boo $  tail $ init $ zip (inits y) (tails y)
        boo (as, bs) =  (a, as ++ (tail bs))
          where
            a   = head bs
            -- as ++ (init bs)

twoOf :: String -> Bool
twoOf xs = any  (\ys -> length ys == 2) $ group $ sort xs

threeOf :: String -> Bool
threeOf xs = any  (\ys -> length ys == 3) $ group $ sort xs
--testInput  = parseLine <$> inputRaw  "input/day2xtest"

type Part = (Int, Int)


parseLine :: String -> Int
parseLine s = case (parse int "" s) of
                Left err -> error (show err)
                Right n  -> n

-- FIXME: create module / contribute parsec-numbers ...
int :: Parser Int
int = ap sign (read <$> many1 digit)

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)



-- it was faster to add those puny instructions by hand
partP :: Parser Part
partP =  (\a b -> (a,b)) <$> int <* char '/' <*> int

---------------------

-- -69453
twice :: [Int] -> (Int, Set Int)
twice xs = foldl' f (0, (S.fromList [])) xs
  where
    f (x, xs) i
      | S.member (x+i) xs = error (show (x+i))
      | otherwise = (x+i, S.insert (x+i) xs)

doChecksum :: Int -> (Char, [Int], Int, [Int]) -> Int
doChecksum n (_, ls, c, rs) = sum (take n ls) + c + (sum (take n rs))

debugMachine :: Int -> (Char, [Int], Int, [Int])
debugMachine n = (s, take n ls, c, take n rs)
    where
      (s, ls, c, rs) = runMachine n

runMachine :: Int -> (Char, [Int], Int, [Int])
runMachine nSteps = foldl' stateF s0 [0..nSteps-1]
  where
    s0 = ('A', cycle [0], 0, cycle [0])

stateF :: (Char, [Int], Int, [Int]) -> a -> (Char, [Int], Int, [Int])
stateF s@('A', _, 0, _) _ = moveRight 'B' 1 s
stateF s@('A', _, 1, _) _ = moveRight 'C' 0 s

stateF s@('B', _, 0, _) _ = moveLeft 'A' 0 s
stateF s@('B', _, 1, _) _ = moveRight 'D' 0 s

stateF s@('C', _, 0, _) _ = moveRight 'D' 1 s
stateF s@('C', _, 1, _) _ = moveRight 'A' 1 s

stateF s@('D', _, 0, _) _ = moveLeft 'E' 1 s
stateF s@('D', _, 1, _) _ = moveLeft 'D' 0 s

stateF s@('E', _, 0, _) _ = moveRight 'F' 1 s
stateF s@('E', _, 1, _) _ = moveLeft 'B' 1 s

stateF s@('F', _, 0, _) _ = moveRight 'A' 1 s
stateF s@('F', _, 1, _) _ = moveRight 'E' 1 s


moveRight :: Char -> Int -> (Char, [Int], Int, [Int]) -> (Char, [Int], Int, [Int])
moveRight s' v' (_, ls, _, (r:rs)) = (s', v':ls, r, rs)

moveLeft :: Char -> Int -> (Char, [Int], Int, [Int]) -> (Char, [Int], Int, [Int])
moveLeft s' v' (_, (l:ls), _, rs) = (s', ls, l, v':rs)

{-
Begin in state A.
Perform a diagnostic checksum after 12399302 steps.



-}
