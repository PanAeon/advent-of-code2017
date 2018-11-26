{-# LANGUAGE LambdaCase #-}

module Day21
    (iterateN
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Loops
import           Data.Char
import           Data.List
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import qualified Numeric                as N
import           System.IO
import           System.IO.Unsafe
import           Text.Parsec.Char       (alphaNum, anyChar, char, digit, oneOf, satisfy, spaces, string)
import           Text.Parsec.Combinator (between, many1, sepBy)
import           Text.Parsec.Prim       (parse, parseTest, try)
import           Text.Parsec.String     (Parser)

import           Debug.Trace


import           Data.Either

inputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)


-- ###/###/### => ###./..../#.#./.#.#
data Pattern = Pattern [String] [String] deriving (Eq, Show)

input :: [Pattern]
input = parseLine <$> inputRaw  "input/day21"

parseLine :: String -> Pattern
parseLine s = case (parse patternP "" s) of
                Left err -> error (show err)
                Right n  -> n

-- FIXME: move to std functions
split :: String -> Char -> [String]
split "" _ = [""]
split (x:xs) c
  | x == c = "": rest
  | otherwise = (x : head rest) : tail rest
 where
   rest = split xs c

patternP :: Parser Pattern
patternP = do
             as <- many1 (oneOf "/#.")
             string " => "
             bs <- many1 (oneOf "/#.")
             pure $ Pattern (split as '/') (split bs '/')

--------------------------------------------------------------------------------

startPattern = ".#."
            ++ "..#"
            ++ "###"

startPatternV = V.fromList startPattern

rotatePattern :: Pattern -> [Pattern]
rotatePattern (Pattern xs ys) = [Pattern (flipH xs) ys]
 where
   flipH xs = reverse <$> xs


genIterations :: Vector Char -> Vector Char
genIterations vs = if even _N
                   then mapV _N 2 find2By2Pattern  vs -- 2x2
                   else mapV _N 3 find3By3Pattern  vs -- 3x3
  where
    _N = truncate $ sqrt (fromIntegral (length vs))


twoByTwoPatterns :: Map String String
twoByTwoPatterns = M.fromList $ (\case (Pattern as bs) -> (join as, join bs)) <$> filter (\case (Pattern as bs) -> length as == 2) input

threeByThreePatterns :: Map String String
threeByThreePatterns = M.fromList $ (\case (Pattern as bs) -> (join as, join bs)) <$> filter (\case (Pattern as bs) -> length as == 3) input


orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = (Just x)
orElse _ b        = b

getOrErr = maybe (error "no such elem") id

find2By2Pattern :: [Char] -> [Char]
find2By2Pattern p = getOrErr $ (M.lookup p twoByTwoPatterns) `orElse`
                    (M.lookup flipped twoByTwoPatterns) `orElse`
                    (M.lookup rot1 twoByTwoPatterns) `orElse`
                    (M.lookup rot2 twoByTwoPatterns) `orElse`
                    (M.lookup rot3 twoByTwoPatterns) `orElse`
                    (M.lookup rot1 twoByTwoPatterns) `orElse`
                    (M.lookup rot2 twoByTwoPatterns) `orElse`
                    (M.lookup rot3 twoByTwoPatterns)
      where
        [a,b,c,d] = p
        flipped = [b,a,d,c]
        rot1 = rotate2By90 p
        rot2 = rotate2By90 rot1
        rot3 = rotate2By90 rot2
        rot1' = rotate2By90 flipped
        rot2' = rotate2By90 rot1'
        rot3' = rotate2By90 rot2'

rotate2By90 :: [a] -> [a]
rotate2By90 [a, b, c, d] = [b,d,a,c]

find3By3Pattern :: [Char] -> [Char]
find3By3Pattern p = getOrErr $ (M.lookup p threeByThreePatterns) `orElse`
                    (M.lookup flipped threeByThreePatterns) `orElse`
                    (M.lookup rot1 threeByThreePatterns) `orElse`
                    (M.lookup rot2 threeByThreePatterns) `orElse`
                    (M.lookup rot3 threeByThreePatterns) `orElse`
                    (M.lookup rot1' threeByThreePatterns) `orElse`
                    (M.lookup rot2' threeByThreePatterns) `orElse`
                    (M.lookup rot3' threeByThreePatterns)
      where
        [a, b, c, d, e, f, g, h, i] = p
        flipped = [c,b,a,f,e,d,i,h,g]
        rot1 = rotate3By90 p
        rot2 = rotate3By90 rot1
        rot3 = rotate3By90 rot2
        rot1' = rotate3By90 flipped
        rot2' = rotate3By90 rot1'
        rot3' = rotate3By90 rot2'

rotate3By90 :: [a] -> [a]
rotate3By90 [a, b, c, d, e, f, g, h, i] = [c,f,i,b,e,h,a,d,g]

rotations  =  sequence_ $ intersperse  (putStrLn " ") (showP <$> [startPattern, flipped, rot1, rot2, rot3, rot1', rot2', rot3'])
      where
        showP xs = sequence_ [ putStrLn (take 3 $ drop (i*3) xs)| i <- [0..2]]
        [a, b, c, d, e, f, g, h, i] = startPattern
        flipped = [c,b,a,f,e,d,i,h,g]
        rot1 = rotate3By90 startPattern
        rot2 = rotate3By90 rot1
        rot3 = rotate3By90 rot2
        rot1' = rotate3By90 flipped
        rot2' = rotate3By90 rot1'
        rot3' = rotate3By90 rot2'



-- mapV :: Int -> Int -> (Vector Int -> Vector Int) -> Vector Int -> Vector Int
mapV :: Int -> Int -> ([t] -> [a]) -> Vector t -> Vector a
mapV _N _M f vs = V.fromList (unwinded >>= id)
  where
    ws = [ getWindow i j | j <- [0.._N-1], i <- [0.._N-1], j `mod` _M == 0, i `mod` _M == 0]
    getWindow i j = [vs V.! (i + di + ((j + dj) * _N)) | dj <- [0.._M-1], di <- [0.._M-1]]
    expanded = f <$> ws
    _K' = (_N `div` _M)
    _N' = _K' * (_M')
    _M' = _M + 1
    unwinded = groupN _K' unwind expanded >>= id
    unwind xs = [ getSlice k xs | k <- [0.._M' - 1]] >>= id
    getSlice k xxs = (\xs -> take _M' (drop (k*_M') xs)) <$> xxs
    --unwinded =


groupN :: Int -> ([a] -> b) -> [a] -> [b]
groupN _ _ [] = []
groupN n f xs = f as : groupN n f bs
   where
     (as,bs) = splitAt n xs



iterateN :: Int -> Vector Char
iterateN n =  head $ drop n (iterate genIterations startPatternV)
{-
vs = V.fromList [0..35] :: Vector Int
mapV 6 3 (\xs -> [0..6] ++ xs ) vs

[16,17,18,19,16,17,18,19,
 20,21,22,23,20,21,22,23,
 24,25,26,27,24,25,26,27,
 28,29,30,31,28,29,30,31,
 16,17,18,19,16,17,18,19,
 20,21,22,23,20,21,22,23,
 24,25,26,27,24,25,26,27,
 28,29,30,31,28,29,30,31]


[16,17,18, 16,17,18, 16,17,18,
 19,20,21, 19,20,21, 19,20,21,
 22,23,24, 22,23,24, 22,23,24,

 16,17,18, 16,17,18, 16,17,18,
 19,20,21, 19,20,21, 19,20,21,
 22,23,24, 22,23,24, 22,23,24,

 16,17,18, 16,17,18, 16,17,18,
 19,20,21, 19,20,21, 19,20,21,
 22,23,24, 22,23,24, 22,23,24]

-}


    -- traverse "windows" -> list of small windows
    -- apply f -> windows of size _M+1
    -- collapse large windows into vector
--
-- mapNN :: Int  -> ([String] -> [String]) -> [String] -> [String]
-- mapNN _M f xs = undefined
--   where
--     z = 3
--
-- mapV :: Int  -> ([String] -> [String]) -> [String] -> [String]
-- mapV _  _ [] = []
-- mapV _M f xs = (f as) ++ mapV _M f bs
--   where
--     (as, bs) = splitAt _M xs
--
-- mapH2 ::  ([String] -> [String]) -> [String] -> [String]
-- mapH2 f xs = (\case (a,b) -> f [a,b]) <$> (zip as bs)
--    where
--      as = head xs
--      bs = head $ tail xs
{-

    If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and convert each 2x2 square into a 3x3 square by following the corresponding enhancement rule.
    Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares, and convert each 3x3 square into a 4x4 square by following the corresponding enhancement rule.

-}
-- hmm, optimize or not ?? that is the question

-- z5 = "#....#..##..#....#.#.#.##.#.#..#.#.#....##.##.......###....###.##.#....#.#.#.##....#.#.#.#....####.#.#....##..##..#....##....##.#.#..#.##..#.#.#.##......#......####.##.##.##.#....##....##..#...#.#.###.#.###.##.....##..##..#....##..##.#.#.#..#.#.#.#.#...##.......##...##.##.##.#....#..##..#....#.#.#.###..#.##.#.#....##.#...."
--      "#....#..##..#....#.#.#.##.#.#..#.#.#....##.##.......###....###.##.#....#.#.#.##....#.#.#.#....####.#.#....##..##..#....##....##.#.#..#.##..#.#.#.##......#......####.##.##.##.#....##....##..#...#.#.###.#.###.##.....##..##..#....##..##.#.#.#..#.#.#.#.#...##.......##...##.##.##.#....#..##..#....#.#.#.###..#.##.#.#....##.#...."

-- 1911767
-- V.length $ V.filter (\x -> x == '#') (iterateN 18) FIXME: print image as bitmap, 2187x2187 pixels
