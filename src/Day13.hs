{-# LANGUAGE LambdaCase #-}
module Day13
    (calculateDelay
    ) where

import           Data.Vector.Unboxed    ()
import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
import           Data.List
import           Text.Parsec.Char       (alphaNum, anyChar, char, digit, satisfy, spaces, string)
import           Text.Parsec.String     (Parser)
-- import           Text.Parsec.String.Char
import           Control.Applicative
import           Control.Monad
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Text.Parsec.Combinator (between, many1, sepBy)
import           Text.Parsec.Prim       (parse, parseTest)

task13InputRaw = unsafePerformIO $ do
                handle <- openFile "input/day13" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task13Input = parseLine <$> task13InputRaw

task13TestInputRaw = [
    "0: 3"
  , "1: 2"
  , "4: 4"
  , "6: 4" ]

task13TestInput = parseLine <$> task13TestInputRaw

parseLine :: String -> (Int, Int)
parseLine s = case (parse lineP "" s) of
                Left err -> error (show err)
                Right n  -> n

int :: Parser Int
int = read <$> many1 digit

lineP :: Parser (Int, Int)
lineP = (\a b -> (a,b)) <$> int <* string ":" <* spaces <*> int <* spaces

----------

data SP = SP (Vector Int) deriving Show
data SC    = SC (Vector Int) deriving Show
data SV = SV (Vector Int) deriving Show

data World = World SC SP SV deriving Show

updateState :: World -> World
updateState (World (SC sc) (SP xs) (SV vs)) =
    World (SC sc) (SP xs') (SV vs')
  where
    n = UV.length sc
    vs' = UV.map g $ UV.zip vs (UV.zip (UV.generate n id) (UV.zip sc xs))
    xs' =  UV.map f $ UV.zip vs' (UV.zip (UV.generate n id) (UV.zip sc xs))
    g (v, (i, (s, x)))
      | x == 0 = 1
      | x == s - 1 = (negate 1)
      | otherwise  = v
    f (v, (i, (s, x)))
      | x < 0  = x
      | x == 0 = 1
      | x == s - 1 = s - 2
      | otherwise = (x + v)


getLength :: World -> Int
getLength (World (SC sc) _ _) = UV.length sc

initialWorld :: [(Int, Int)] -> World
initialWorld ys =  World (SC sc) (SP xs) (SV vs)
  where
    n  = (maximum $ fst <$> ys) + 1
    m  = M.fromList ys
    sc = UV.fromList [ maybe (negate 1) id $ M.lookup i m | i <- [0..n-1]]
    xs = UV.fromList [ maybe (negate 1) (const 0) $ M.lookup i m | i <- [0..n-1]]
    vs = UV.replicate n 1


-- That's not the right answer.  570048
calculateDelay = head $ dropWhile (\x -> (snd x) > 0 ) ((\d -> (d, calculateScoreNormally d vs )) <$> [0..])
  where
    (World (SC vs) _ _)  = initialWorld task13Input
    -- hs = UV.toList vs

calculateScoreNormally :: Int -> Vector Int -> Int
calculateScoreNormally delay heights  = UV.sum score
  where
    n   = UV.length heights
    zs  = UV.zip heights (UV.generate n (\i -> i - 1))
    score = UV.map (\case (h,i) -> sc (i+1) (1+i+delay) h) zs
    -- score = [ sc (i+1) t h | (h, i) <- zip heights [-1..n-2], let t = 1 + i + delay]
    sc k t h = if h > 0 && t `mod` (2 * (h-1)) == 0
               then 1
               else 0

{-
calculateScoreNormally delay heights  = UV.sum score
  where
    n   = length heights
    score = [ sc (i+1) t h | (h, i) <- zip heights [-1..n-2], let t = 1 + i + delay]
    sc k t h = if h > 0 && t `mod` (2 * (h-1)) == 0
               then k * h
               else 0
-}

calculateScore :: Int -> [(Int, Int)] -> Int
calculateScore delay ys = sum $ score <$> ys''
  where
    iw  = initialWorld ys
    n   = getLength (iw)
    ys' = drop delay $ iterate updateState (iw)
    ys'' = zip [-1..n] (ys')
    score (p, (World (SC sc) (SP xs) _)) =
      if p >= 0  && xs UV.! (p + 1) == 0
      then (p + 1) * (sc UV.! (p + 1))
      else 0
-- 2972 is too high
-- 1960 is the right answer
-- part II your answer is too low 29508
-- 29509 is too low
-- 209688 is too low
-- 3903378
showN :: [(Int, Int)] -> Int -> IO ()
showN ys n = (putStrLn . printWorld (n-1)) $ head $ (drop n $ iterate updateState (initialWorld ys))


printWorld  :: Int -> World -> String
printWorld p (World (SC sc') (SP xs') _) =
    firstRow ++ rest
  where
    sc = UV.toList sc'
    xs = UV.toList xs'
    firstRow = intercalate " " [ " " ++ show i ++ " " | i <- [0..(length xs - 1)]] ++ "\n"
    maxL = maximum sc
    rest = intercalate "\n" [ printRaw y  | y <- [0..maxL-1]]
    printRaw y = intercalate " " [printCell y h x i |(i, (h,x)) <- zip [0..] (zip sc xs)]

    printCell y h x i
      | y == 0 && i == p && y == x = "(S)"
      | y == 0 && i == p = "( )"
      | h == (negate 1) = "   "
      | y == x = "[S]"
      | y < h = "[ ]"
      | otherwise = "   "
