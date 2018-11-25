module Day19(example, followPath) where

import           Data.Vector         (Vector)
import qualified Data.Vector as V
import           System.IO
import           System.IO.Unsafe
import           Data.Char
import           Data.List
import           Text.Parsec.Char            (alphaNum, anyChar, char, digit, oneOf, satisfy, spaces, string)
import           Text.Parsec.String          (Parser)
import           Control.Applicative
import Control.Monad.Loops
import           Control.Monad
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust)
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import qualified Numeric                     as N
import           Text.Parsec.Combinator      (between, many1, sepBy)
import           Text.Parsec.Prim            (parse, parseTest, try)


import Debug.Trace

-- FIXME: move out vector generic, mutable snippets, with usage
-- FIXME: iterate util, move out
-- FIXME: parser utils / examples
-- FIXME: maybe create empty tests, just in case

-- import  Control.Monad.Primitive(PrimMonad)
import Data.Either

task18InputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task18Input :: Vector (Vector Char)
task18Input = V.fromList $ convertLine <$> task18InputRaw  "input/day19"

example :: Vector (Vector Char)
example = V.fromList $ convertLine <$> task18InputRaw  "input/day19example"

convertLine :: String -> Vector Char
convertLine xs = V.fromList xs

-----------------------------------------------------------

data Dir = Down | Up | Lt | Rt deriving (Eq,Show)
data St  = St Dir Int Int deriving (Eq,Show)
--------------------------------------------------------------------------------
-- confused, convert to graph or not? (lets' not convert for now)
unsafeGet :: (Maybe a) -> a
unsafeGet (Just a) = a

findStart :: Vector (Vector Char) -> (Int, Int)
findStart xxs = (unsafeGet $ V.findIndex (=='|') (V.head xxs), 0)

type Treasure =  (Int, Int, Vector (Vector Char))

followPath ::  Vector (Vector Char) -> ([Char], Int)
followPath xxs =  (filter isAlpha res, length res)
   where
     (x0, y0) = findStart xxs
     _H = V.length xxs
     _W = traceShowId $ V.maximum $ V.map (V.length) xxs
     xxs' = (\vs -> vs V.++ (V.replicate (_W - V.length vs) ' '))<$> xxs
     res = unfoldr (move (_W, _H, xxs')) (St Down x0 y0)



-- HATBMQJYZ
-- + how many steps
move :: Treasure -> St -> Maybe (Char, St)
move (_W, _H, xss) (St d x y) = traceShowId $ f <$> getMoves (_W, _H, xss) d  (x,y)
     where
       f (i,j,d') = (get i j, St d' i j)
       get i j = V.unsafeIndex (V.unsafeIndex xss j) i



getMoves :: Treasure -> Dir -> (Int, Int) -> Maybe (Int, Int, Dir)
getMoves (_W, _H, xss) d (x,y) =  fmap getCoords $ maybeHead $ filter isValid $ filter inBounds $ [ (x+dx, y+dy, d', dx, dy) | (dx,dy,d') <- deltas]
   where
     getCoords  (i,j, d', di, dj) = (i,j, d')
     maybeHead [] = Nothing
     maybeHead (x:_) = Just x
     get i j = V.unsafeIndex (V.unsafeIndex xss j) i
     isValid (i,j, d', di, dj)
             | dj == 0 = let ch = get i j in ch /= ' '-- ch == '-' || ch == '+' || isAlpha ch
             | di == 0 = let ch = get i j in ch /= ' '--ch == '|' || ch == '+' || isAlpha ch
     inBounds (i,j,d',di,dj) = j > 0 && j < _H && i > 0 && i < _W
     deltas = case d of
                   Up -> [(0, negate 1, Up), (1, 0, Rt), (negate 1, 0, Lt)]
                   Down -> [(0, 1, Down), (1, 0, Rt), (negate 1, 0, Lt)]
                   Lt -> [(negate 1, 0, Lt), (0, negate 1, Up), (0,1, Down)]
                   Rt -> [(1, 0, Rt), (0, negate 1, Up), (0,1, Down)]


-- 25 minutes, nothing ready )) I'm a bit creamed
