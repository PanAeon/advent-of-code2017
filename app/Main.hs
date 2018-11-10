module Main where

import           Day12
import           Day13
import           Day14
import           Day16
import           Day18

-- FIXME: write fast dfs, why it can't handle 16k nodes?
main :: IO ()
main = putStrLn $ show $ runProgram task18Input

  --putStrLn $ show $ calculateDelay
