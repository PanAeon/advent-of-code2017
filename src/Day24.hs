{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

module Day24
    (
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

-- simplify, take just iterate, single var, st ... and check them separately

inputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

input = parseLine <$> inputRaw  "input/day24"
--myInput  = parseLine <$> inputRaw  "input/day23my"

type Part = (Int, Int)


parseLine :: String -> Part
parseLine s = case (parse partP "" s) of
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

naive :: Int -> [Part] -> [Part]
naive port [] = []
naive port xs = if null ys
                then []
                else maximumBy (comparing sumEl) $ fmap (\p ->
                       let (a',b') = orient p
                       in (a',b') : naive b' (delete p xs)
                     ) ys
     where
       ys = filter isConn xs
       sumEl xs = sum $ fmap (uncurry (+)) xs
       isConn (a,b) = port == a || port == b
       orient (a,b)
          | a == port = (a,b)
          | otherwise = (b,a)

-- 1511, like that

allLengthes :: [Part] -> [([Part], Int)]
allLengthes xs = (\xs -> (xs, sumEl xs)) <$> filter (\xs -> length xs == ml) zs
   where
     zs = (fmap (\p ->
            let (a',b') = orient p
            in (a',b') : naivel b' (delete p xs)
          ) ys)
     ml = length $ maximumBy (comparing lengthel) zs
     ys = filter isConn xs
     sumEl xs = sum $ fmap (uncurry (+)) xs
     isConn (a,b) = 0 == a || 0 == b
     lengthel xs = length xs
     orient (a,b)
        | a == 0 = (a,b)
        | otherwise = (b,a)

naivel :: Int -> [Part] -> [Part]
naivel port [] = []
naivel port xs = if null ys
                then []
                else maximumBy (comparing lengthel) $ fmap (\p ->
                       let (a',b') = orient p
                       in (a',b') : naivel b' (delete p xs)
                     ) ys
     where
       ys = filter isConn xs
       lengthel xs = length xs
       isConn (a,b) = port == a || port == b
       orient (a,b)
          | a == port = (a,b)
          | otherwise = (b,a)
