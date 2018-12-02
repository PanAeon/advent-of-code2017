module Main where


import           Day2x

-- FIXME: write fast dfs, why it can't handle 16k nodes?
main :: IO ()
main = putStrLn $ show $  bar input --runDuet taskInput
  where
    -- (_, _, _, n) = runEnchancedVirus 10000000 input

  --putStrLn $ show $ calculateDelay
