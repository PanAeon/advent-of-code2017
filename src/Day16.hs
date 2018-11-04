module Day16
    (baz, task16Input
    ) where



import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
import           Data.List
import           Text.Parsec.Char       (alphaNum, anyChar, oneOf, char, digit, satisfy, spaces, string)
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
import qualified Data.Bits as B

task16InputRaw = unsafePerformIO $ do
                handle <- openFile "input/day16" ReadMode
                contents <- hGetContents handle
                pure $ (head $ lines contents)

task16Input = parseLine task16InputRaw

parseLine :: String -> [Move]
parseLine s = case (parse nodeP "" s) of
                Left err -> error (show err)
                Right n  -> n

int :: Parser Int
int = read <$> many1 digit


name :: Parser Char
name = oneOf ['a'..'p']

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char deriving (Show, Eq)

nodeP :: Parser [Move]
nodeP = (sepBy  moveP (char ',' <* spaces) )

moveP :: Parser Move
moveP = (Spin <$> (char 's' *> int) ) <|>
        (Exchange <$> (char 'x' *> int <* char '/') <*> int) <|>
        (Partner <$> (char 'p' *> name <* char '/') <*> name )
---------------------------------------------------------------------
numP = 16
testInput = parseLine "s1,x3/4,pe/b"

pos0 :: (Int, Map Char Int)
pos0 = (i,m)
  where
    i = foldl' f 0 [ (i, 4 * (15-i))| i <- [0..15]]
    f :: Int -> (Int, Int) -> Int
    f x (i,p) = x B..|. (B.rotateL p i)
    m = M.fromList [ (p, i) | (i,p) <- zip [0..]['a'..'p']]
-- toRepr :: String -> (Int, Map Char Int)
-- toRepr xs = undefined
--   where


-- consider vector ..
transform :: (Int, Map Char Int) -> Move -> (Int, Map Char Int)
transform (n', m) (Spin n) = (B.rotateR (n * 4) n', m)  -- FIXME: rotate wrong..
transform (n, m) (Exchange i j) = (n'',m) -- also wrong ..f :)
  where
    bitMI = B.bit (4*i) B..|. B.bit (4*i+1) B..|. B.bit(4*i+2) B..|. B.bit(4*i+3)
    bitMJ = B.bit (4*j) B..|. B.bit (4*j+1) B..|. B.bit(4*j+2) B..|. B.bit(4*j+3)
    bitsI = n B..&. bitMI
    bitsJ = n B..&. bitMJ
    n' = n B..&. (B.complement bitMI) B..&. (B.complement bitMJ) -- zeroed target bits
    bitsI' = B.rotate (i-j) bitsI
    bitsJ' = B.rotate (j-i) bitsJ
    n'' = n' B..|. bitsI' B..|. bitsJ'

-- transform xs (Partner a b) = if a == b then xs
--                                        else ps ++ (b':ds) ++ (a': bs)
--   where
--       (ps, (a':as)) = span (\x -> x /= a && x /= b) xs
--       (ds, (b':bs)) = span (\x -> x /= a && x /= b) as

dance :: String -> [Move] -> String
dance a0 xs = undefined -- foldl' transform a0 xs

foo = "nlciboghjmfdapek"

baz :: String -> [Move] -> String
baz a0 ms =  last $ take 1000000000 $ tail $ iterate trans a0
  where
    trans (a0:a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:[]) =
      [a12, a4, a2, a11, a14, a10, a6, a7, a3, a8, a15, a1, a9, a0, a5, a13]
    -- trans xs = fst <$> sortOn snd (zip xs [12,4,2,11,14,10,6,7,3,8,15,1,9,0,5,13])

unsafeGet (Just x) = x
tranformX :: [Int]
tranformX = (\x -> unsafeGet $ elemIndex x "nlciboghjmfdapek") <$> ['a'..'p']
-- one but last
--kicmdbghanejpflo -- it's not the right answer
-- pdcjleghmaoinkbf wrong (it's off by 1)
