{-# LANGUAGE LambdaCase #-}
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
import           Text.Parsec.Char       (alphaNum, anyChar, char, digit, oneOf, satisfy, spaces, string)
import           Text.Parsec.String     (Parser)
-- import           Text.Parsec.String.Char
import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import           Data.Set               (Set)
import qualified Data.Set               as S
import qualified Numeric                as N
import           Text.Parsec.Combinator (between, many1, sepBy)
import           Text.Parsec.Prim       (parse, parseTest)

task16InputRaw = unsafePerformIO $ do
                handle <- openFile "input/day16" ReadMode
                contents <- hGetContents handle
                pure $ (head $ lines contents)

task16Input = parseLine task16InputRaw

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char deriving (Show, Eq)

parseLine :: String -> [Move]
parseLine s = case (parse nodeP "" s) of
                Left err -> error (show err)
                Right n  -> n


int :: Parser Int
int = read <$> many1 digit


name :: Parser Char
name = oneOf ['a'..'p']

nodeP :: Parser [Move]
nodeP = (sepBy  moveP (char ',' <* spaces) )

moveP :: Parser Move
moveP = (Spin <$> (char 's' *> int) ) <|>
        (Exchange <$> (char 'x' *> int <* char '/') <*> int) <|>
        (Partner <$> (char 'p' *> name <* char '/') <*> name )
---------------------------------------------------------------------
numP = 16
testInput = parseLine "s1,x3/4,pe/b"

pos0 :: Int
pos0 = foldl' f 0 [ (i, 4 * i)| i <- [0..15]]
  where
    f x (i,p) = x .|. (i `rotateL` p)

-- position reversed inside int64
toPos :: Int -> String -> Int
toPos _N xs = foldl' f 0 [ (ord c - 97, 4 * i)| (i,c) <- zip [0..(_N-1)] xs]
  where
    f x (i,p) = x .|. (i `rotateL` p)

fromPos :: Int -> Int -> String
fromPos _N x = [ chr ((x `rotateR` (4*i) .&. 15) + 97)   | i <- [0..(_N - 1)]]

elemIndex' a as = maybe (error "no such index") id (elemIndex a as)

unpackLabels :: Int -> Int -> Map Char Char
unpackLabels _N x = M.fromList zs
  where
    as = take _N ['a'..'p']
    bs = fromPos _N x
    zs = (\c -> (c, as !! (elemIndex' c bs))) <$> as

-- blank -- pos0
swapLabels :: Int -> Int -> Move -> Int
swapLabels _N x (Partner a b) = transform _N x (Exchange i j)
   where
     i = ord a - 97
     j = ord b - 97

-- _N - 16
transform :: Int -> Int -> Move -> Int
transform _N x (Spin n) = (x' .|. x'') .&. ( (1 `shiftL` (_N * 4))- 1 )
  where
    r' = (n `mod` _N) * 4
    l' = (_N*4) - r'
    x'  =  x `rotateL` r'
    x'' =  x `rotateR` l'

transform _N x (Exchange i j) = (x' .|. bitsI .|. bitsJ)  .&. ( (1 `shiftL` (_N * 4))- 1 )
  where
    i'     = 4 * i
    j'     = 4 * j
    bitMI  =  complement (15 `rotateL` i')
    bitMJ  =  complement (15 `rotateL` j')

    bitsI  = (x `rotateR` i' .&. 15) `rotateL` (j')
    bitsJ  = (x `rotateR` j' .&. 15) `rotateL` (i')

    x' = x .&. bitMI .&. bitMJ




-- transform xs (Partner a b) = if a == b then xs
--                                        else ps ++ (b':ds) ++ (a': bs)
--   where
--       (ps, (a':as)) = span (\x -> x /= a && x /= b) xs
--       (ds, (b':bs)) = span (\x -> x /= a && x /= b) as

showBits x = (replicate k '0') ++ s
  where
    s = N.showIntAtBase (2::Int) (chr . (48+)) (x) ""
    k = 64 - length s

grouped :: Int -> [a] -> [[a]]
grouped n [] = []
grouped n xs = as:(grouped n bs)
   where
     (as,bs) = splitAt n xs

printBits :: Int -> String
printBits x = intercalate " " ys''
  where
    xs = iterate (\case (l,r) -> (l `rotateR` 1, l .&. 1) ) (x, 0)
    ys = reverse $ fmap snd $ take 64  $ drop 1 xs
    -- ys' = reverse $ replicate 0 (64 - (length ys)) ++ ys
    toChar 0 = '0'
    toChar 1 = '1'
    ys'' = (\xs -> toChar <$> xs)  <$> grouped 4 ys


{-

FIXME: how to convert below nicely? bind + Reader?
 def stringifyBits(l:Long): String = {
      Iterator
        .iterate((l, 0l)) { case (l,r) => (l >>> 1l, l & 1l) }
        .drop(1)
        .takeWhile(_._1 >= 0)
        .take(64)
        .map(_._2)
        .toList
        .padTo(64, 0l)
        .reverse
        .grouped(8)
        .map(_.mkString(""))
        .mkString(" ")
    }
-}

-- (transform _N)
dance ::  Int -> (Int -> Move -> Int) -> [Move] -> Int
dance a0 f xs =  foldl' f  a0 xs

isPartner  (Partner _ _) = True
isPartner _              = False

wholeDance :: Int -> String -> [Move] -> (String, Int, Int)
wholeDance _N s moves = (s'', indexes, labels)
  where
    x0 = toPos _N s
    p0 = pos0 .&. ( (1 `shiftL` (_N * 4))- 1 )
    indexes = dance x0 (transform _N) (filter (not . isPartner) moves)
    labels  = dance p0 (swapLabels _N) (filter isPartner moves)
    labelsMap = unpackLabels _N labels
    s' = fromPos _N indexes
    s'' = (labelsMap M.!) <$> s'


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
