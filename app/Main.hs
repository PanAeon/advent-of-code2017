module Main where


import           Day22

-- FIXME: write fast dfs, why it can't handle 16k nodes?
main :: IO ()
main = putStrLn $ show $ n
  where
    (_, _, _, n) = runEnchancedVirus 10000000 input

  --putStrLn $ show $ calculateDelay
