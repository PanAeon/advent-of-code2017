module Main where

import           Day12
import           Day13
import           Day14
import           Day16

-- FIXME: write fast dfs, why it can't handle 16k nodes?
main :: IO ()
main = putStrLn $ show $ iterateStrip 16 1000000000

  --putStrLn $ show $ calculateDelay
