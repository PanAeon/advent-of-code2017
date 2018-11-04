module Main where

import           Day13
import Day14
import Day12
import Day16

-- FIXME: write fast dfs, why it can't handle 16k nodes?
main :: IO ()
main = putStrLn $ show $ baz "nlciboghjmfdapek" task16Input

  --putStrLn $ show $ calculateDelay
