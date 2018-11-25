module Main where


import           Day19

-- FIXME: write fast dfs, why it can't handle 16k nodes?
main :: IO ()
main = putStrLn $ show $ followPath example

  --putStrLn $ show $ calculateDelay
