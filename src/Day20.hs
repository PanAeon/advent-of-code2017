module Day20() where

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

day20InputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

day20Input :: [PointD]
day20Input = parseLine <$> day20InputRaw  "input/day20"

-- example :: Vector (Vector Char)
-- example = V.fromList $ convertLine <$> task18InputRaw  "input/day19example"

parseLine :: String -> PointD
parseLine s = case (parse pointP "" s) of
                Left err -> error (show err)
                Right n  -> n


int :: Parser Integer
int = ap sign (read <$> many1 digit)

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)
--
--
-- name :: Parser Char
-- name = oneOf ['a'..'p']
--
-- nodeP :: Parser [Move]
-- nodeP = (sepBy  moveP (char ',' <* spaces) )
coordP :: Parser (Integer, Integer, Integer)
coordP = (\a b c -> (a,b,c)) <$> ((char '<') *> int <* (char ',')) <*>
                      (int <* (char ',')) <*>
                      (int <* (char '>'))

pointP :: Parser PointD
pointP = PointD <$> (string "p=" *> coordP) <*>
                    (string ", v=" *> coordP) <*>
                    (string ", a=" *> coordP)

  -- (Spin <$> (char 's' *> int) ) <|>
  --       (Exchange <$> (char 'x' *> int <* char '/') <*> int) <|>
  --       (Partner <$> (char 'p' *> name <* char '/') <*> name )

-- a bit tired, wan't make a race
-----------------------------------------------------------
data PointD = PointD (Integer,Integer,Integer) (Integer, Integer, Integer) (Integer, Integer, Integer) deriving (Eq, Show)

testPoint = PointD (-3787,-3683,3352) (41,-25,-124) (5,9,1)
testPointB = PointD (6815,2269,3786) (-93,23,38) (-8,-6,-10)

positionAt :: Integer -> PointD -> PointD
positionAt t (PointD (x0,y0,z0) (vx,vy,vz) (ax, ay, az)) = p'
  where
    fv v0 a = v0 + a*t
    fx x0 v0 a = x0 + v0*t + ((a * t * t) `div` 2)
    p' = PointD (fx x0 vx ax, fx y0 vy ay, fx z0 vz az) (fv vx ax, fv vy ay, fv vz az) (ax, ay, az)

-- x = x0 + v0*t + 1/2 (a*t^2)

manhattan (PointD (x,y,z) _ _ ) = x*x + y*y + z*z

rankPoints :: Integer -> (Integer, PointD)
rankPoints t = head rs
  where
    xs' = positionAt t <$> day20Input
    ys' = zip [0..] xs'
    rs  = sortOn f ys'
    f (i,p) = manhattan p

----------------- math power             ! -------------------------------------


day20TestInput = [
   PointD (-6,0,0) (3,0,0) (0,0,0),
   PointD (-4,0,0) (2,0,0) (0,0,0),
   PointD (-2,0,0) (1,0,0) (0,0,0),
   PointD (3,0,0) (-1,0,0) (0,0,0)
 ]

day20TestInput' = [
   PointD (0,0,0) (0,10,0) (3,0,0),
   PointD (1000,0,0) (0,5,0) (-4,0,0)
 ]

solveForDiscreteT :: (Double, Double, Double) -> Maybe [Double]
solveForDiscreteT (a',b',c') = xs'
  where
    det = sqrt ((b'*b') - (4 * a' * c'))
    xs = if (abs a') < 0.0001 && (abs b') < 0.0001
         then if (abs c' < 0.0001)
              then Nothing -- any
              else Just [] -- None
         else if a' /= 0
           then Just [((negate b') + det) / (2 * a'), ((negate b') - det) / (2 * a')]
           else Just [ negate (c' / b')]
    xs' = (\xs -> filter (>= 0) $ filter (not . isNaN) $ filter (not . isInfinite) xs) <$> xs

intersectUpToSingleDec :: [Double] -> [Double] -> [Double]
intersectUpToSingleDec xs ys = (closeTo ys xs) ++ ( closeTo xs ys)
   where
     closeTo as bs = filter (\b -> any (\a -> abs (b - a) < 1 ) as) bs

intersectAll :: [Double] -> [Double] -> [Double]
intersectAll xs ys = xs ++ ys
     -- closeToAsIntegral as bs  = filter (\b -> any (\a -> abs (b - a) < 1 ) as) bs
     -- xs' = (\x -> ceiling (0.1 * x)) <$> xs
     -- ys' = (\x -> ceiling (0.1 * x)) <$> ys
toIntegerT :: [Double] -> [Integer]
toIntegerT xs = nub $ xs >>= (\x -> [floor x, (ceiling x)])


--  Once particles collide, they are removed and cannot collide with anything else after that tick.
--  maybe this is the problem?
--

positionP :: Double -> PointD -> (Double, Double, Double)
positionP t (PointD (x0,y0,z0) (vx,vy,vz) (ax, ay, az)) = p'
  where
    fx x0 v0 a = (fromIntegral x0) + ((fromIntegral v0 + ((fromIntegral a) / 2))*t) + (((fromIntegral a) * t * t) / 2)
    p' =  (fx x0 vx ax, fx y0 vy ay, fx z0 vz az)

pd :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
pd (x0, y0, z0) (x1, y1, z1) = (abs (x0-x1) < 1.0) &&
                               (abs (y0-y1) < 1.0) &&
                               (abs (z0-z1) < 1.0)

dist'  :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dist' (x0, y0, z0) (x1, y1, z1) =  sqrt (( (x0-x1)^2) +
                               ((y0-y1)^2) +
                               ((z0-z1)^2))

-- 293 colliding so far
findTiming p0 xs =  nub $ (xs >>= f) >>= g
  where
    f p1 = let
             cx = solveForDiscreteT $ toCoefX p0 p1
             cy = solveForDiscreteT $ toCoefY p0 p1
             cz = solveForDiscreteT $ toCoefZ p0 p1
             is = case (cx, cy, cz) of
               (Just cx, Just cy, Just cz) -> (cx `intersectUpToSingleDec` cy `intersectUpToSingleDec` cz)
               (Just cx, Nothing, Just cz) -> (cx `intersectUpToSingleDec` cz)
               (Just cx, Nothing, Nothing) -> cx
               (Just cx, Just cy, Nothing) -> cx `intersectUpToSingleDec` cy
               (Nothing, Just cy, Just cz) -> ( cy `intersectUpToSingleDec` cz)
               (Nothing, Nothing, Just cz) -> cz
               (Nothing, Just cy, Nothing) -> cy
               (Nothing, Nothing, Nothing) -> [0.0] --not empty, but colliding at start !!
                -- [Double]
           in (\t -> (fromIntegral t,p1)) <$> (toIntegerT is)
           -- (\t -> (t,p1)) <$> is
           -- (\t -> (fromIntegral t,p1)) <$> (toIntegerT is)
    g (t,p1) = if (pd p0' p1')
               then [t]
               else []
      where
        p0' = positionP t p0
        p1' = positionP t p1

findColliding xs = filter (not . null) $ (\p ->  (findTiming p (delete p xs))) <$> xs

-- 707, cs = [ x | x <- xs, if x collides p]
-- if null cs then z else ts
collider xs = foldl eliminate xs ts
  where
    -- ys = nub $ xs >>= (\p ->  (findTiming p (delete p xs)))
    -- ts = sort ys
    eliminate xs' t = filter (\p -> not $ collidesAt t p (delete p xs')) xs'
    collidesAt t p zs = any pointCollides zs
       where
         pointCollides p1 = pd p0' p1'
           where
             p0' = positionP t p
             p1' = positionP t p1
    ts = [10.0,11.0,12.0,14.0,15.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0,33.0,34.0,35.0,37.0,38.0]
-- findTiming = filter (\p -> null (testTiming p day20Input)) day20Input
-- 211 too low

toCoefX :: PointD -> PointD -> (Double, Double, Double)
toCoefX (PointD (x0,_, _) (vx0, _, _) (ax0, _, _) ) (PointD (x1,_, _) (vx1, _, _) (ax1, _, _) ) = (a,b,c)
  where
    a = fromIntegral (ax0 - ax1) / 2
    b =  (fromIntegral vx0 - (fromIntegral vx1) + ((fromIntegral ax0) / 2) - ((fromIntegral ax1) / 2))
    c = fromIntegral (x0 - x1)

toCoefY :: PointD -> PointD -> (Double, Double, Double)
toCoefY (PointD (_, x0, _) (_, vx0,  _) (_, ax0,  _) ) (PointD (_,x1, _) (_, vx1,  _) (_, ax1,  _) ) = (a,b,c)
  where
    a = fromIntegral (ax0 - ax1) / 2
    b =  (fromIntegral vx0 - (fromIntegral vx1) + ((fromIntegral ax0) / 2) - ((fromIntegral ax1) / 2))
    c = fromIntegral (x0 - x1)

toCoefZ :: PointD -> PointD -> (Double, Double, Double)
toCoefZ (PointD (_, _, x0) ( _, _, vx0) ( _, _, ax0) ) (PointD (_, _, x1) ( _, _, vx1) ( _, _, ax1) ) = (a,b,c)
  where
    a = fromIntegral (ax0 - ax1) / 2
    b = (fromIntegral vx0 - (fromIntegral vx1) + ((fromIntegral ax0) / 2) - ((fromIntegral ax1) / 2))
    c = fromIntegral (x0 - x1)
