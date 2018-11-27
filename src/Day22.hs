{-# LANGUAGE LambdaCase #-}
module Day22
    (runEnchancedVirus, testInput, input, GameMap
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Loops
import           Data.Char
import           Data.List
import           Data.Map.Lazy          (Map)
import qualified Data.Map.Lazy          as M
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


input = parseInput $ inputRaw  "input/day22"
testInput = parseInput $ inputRaw  "input/day22example"

type GameMap = Map (Int, Int) Int

parseInput :: [String] -> GameMap
parseInput xxs = gameMap
  where
    n   = length xxs
    r   = n `div` 2
    l   = negate r
    gameMap = M.fromList [((i,j), 1) | (j,xs) <- (zip (reverse [l..r]) xxs), (i,x) <- (zip [l..r] xs), x=='#']



    -- Clean nodes become weakened.
    -- Weakened nodes become infected. 2
    -- Infected nodes become flagged. 1
    -- Flagged nodes become clean. 3
infected = 1
weakened = 2
flagged  = 3

-- ok, parsing was quick
--------------------------------------------------------------------------------
data Dir = Up | Dn | Lt | Rt deriving (Eq, Show) -- TODO: think how to repr more compactly

turnRight Up = Rt
turnRight Rt = Dn
turnRight Dn = Lt
turnRight Lt = Up

turnLeft Up = Lt
turnLeft Lt = Dn
turnLeft Dn = Rt
turnLeft Rt = Up

reverseDir Up = Dn
reverseDir Dn = Up
reverseDir Lt = Rt
reverseDir Rt = Lt

move (x,y) Up = (x, y + 1)
move (x,y) Dn = (x, y - 1)
move (x,y) Rt = (x + 1, y)
move (x,y) Lt = (x - 1, y)

singleStep :: ((Int,Int), Dir, GameMap, Int) -> ((Int, Int), Dir, GameMap, Int)
singleStep (p, dir, m, n)
    | M.member p m = let  -- turn right, then move
                       d' = turnRight dir
                       p' = move p d'
                     in (p', d', M.delete p m, n)
    | otherwise    = let  -- turn right, then move
                       d' = turnLeft dir
                       p' = move p d'
                     in (p', d', M.insert p 1 m, n + 1)

runVirus :: Int -> GameMap -> ((Int, Int), Dir, GameMap, Int)
runVirus n m = head $ drop n $ iterate singleStep ((0,0), Up, m, 0)

-- well, it was like 20 minute task, now the hard part
--------------------------------------------------------------------------------

enchancedStep :: ((Int,Int), Dir, GameMap, Int) -> ((Int, Int), Dir, GameMap, Int)
enchancedStep (p, dir, m, n) = case maybeNode of
                                 (Just 1) -> let
                                                      d' = turnRight dir
                                                      p' = move p d'
                                                    in (p', d', M.insert p flagged m, n)
                                 (Just 2) -> let
                                                      d' = dir
                                                      p' = move p d'
                                                    in (p', d', M.insert p infected m, n+1)
                                 (Just 3) ->  let
                                                      d' = reverseDir dir
                                                      p' = move p d'
                                                    in (p', d', M.delete p m, n)
                                 Nothing  -> let
                                                    d' = turnLeft dir
                                                    p' = move p d'
                                             in (p', d', M.insert p weakened m, n)
  where
    maybeNode = M.lookup p m


runEnchancedVirus :: Int -> GameMap -> ((Int, Int), Dir, GameMap, Int)
runEnchancedVirus n m = head $ drop n $ iterate enchancedStep ((0,0), Up, m, 0)
