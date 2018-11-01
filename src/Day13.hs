module Day13
    (
    ) where


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

data ScannersPositions = ScannersPositions [Int] deriving Show
data ScannersConfig    = ScannersConfig [Int] deriving Show
data SV = SV [Int] deriving Show

data World = World Int ScannersConfig ScannersPositions SV deriving Show

updateState :: World -> World
updateState (World p (ScannersConfig sc) (ScannersPositions xs) (SV vs)) =
    World (p+1) (ScannersConfig sc) xs' (SV vs')
  where
    vs' = g <$> zip vs (zip [0..] (zip sc xs))
    xs' = ScannersPositions $ f <$> zip vs' (zip [0..] (zip sc xs))
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
getLength (World _ (ScannersConfig sc) _ _) = length sc

initialWorld :: [(Int, Int)] -> World
initialWorld ys =  World (negate 1) (ScannersConfig sc) (ScannersPositions xs) (SV vs)
  where
    n  = (maximum $ fst <$> ys) + 1
    m  = M.fromList ys
    sc = [ maybe (negate 1) id $ M.lookup i m | i <- [0..n-1]]
    xs = [ maybe (negate 1) (const 0) $ M.lookup i m | i <- [0..n-1]]
    vs = replicate n 1


showN :: [(Int, Int)] -> Int -> IO ()
showN ys n = (putStrLn . printWorld) $ head $ (drop n $ iterate updateState (initialWorld ys))

calculateDelay = dropWhile (\x -> (snd x) > 0 ) ((\d -> (d, calculateScore d task13Input)) <$> [0..2000])

calculateScore :: Int -> [(Int, Int)] -> Int
calculateScore delay ys = sum $ score <$> ys''
  where
    ys' = drop delay $ iterate updateState (initialWorld ys)
    packetInside (World p (ScannersConfig sc) _ _) = p < (length sc)
    n = getLength (initialWorld ys)
    ys'' = zip [-1..n-2] (ys')
    score (p, (World z (ScannersConfig sc) (ScannersPositions xs) _)) =
      if p >= 0  && xs !! (p + 1) == 0
      then (p + 1) * (sc !! (p + 1))
      else 0
-- 2972 is too high
-- 1960 is the right answer


printWorld  :: World -> String
printWorld (World p (ScannersConfig sc) (ScannersPositions xs) _) =
    firstRow ++ rest
  where
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
