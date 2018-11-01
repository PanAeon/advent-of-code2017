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

data World = World Int ScannersConfig ScannersPositions deriving Show

updateState :: World -> World
updateState (World p (ScannersConfig sc) (ScannersPositions xs)) =
    World (p+1) (ScannersConfig sc) xs'
  where
    xs' = ScannersPositions $ f <$> zip [0..] (zip sc xs)
    f (i, (s, x))
      | x < 0  = x
      | x >= s = 0
      | otherwise = x + 1


initialWorld :: [(Int, Int)] -> World
initialWorld ys =  (World (negate 1) (ScannersConfig sc) (ScannersPositions xs))
  where
    n  = (maximum $ fst <$> ys) + 1
    m  = M.fromList ys
    sc = [ maybe (negate 1) id $ M.lookup i m | i <- [0..n-1]]
    xs = [ maybe (negate 1) (const 0) $ M.lookup i m | i <- [0..n-1]]


printWorld  :: World -> String
printWorld (World p (ScannersConfig sc) (ScannersPositions xs)) =
    firstRow ++ rest
  where
    firstRow = intercalate " " [ " " ++ show i ++ " " | i <- [0..(length xs - 1)]] ++ "\n"
    maxL = maximum sc
    rest = intercalate "\n" [ printRaw y  | y <- [0..maxL-1]]
    printRaw y = intercalate " " [printCell y h x |(h,x) <- zip sc xs]

    printCell y h x
      | h == (negate 1) = "   "
      | y == x = "[S]"
      | y < h = "[ ]"
      | otherwise = "   "
