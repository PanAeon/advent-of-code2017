{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Day17(iterateSpin') where



realInput = 371
testInput = 3

-- let's make current pos implicit (0)
singleIter :: Int -> ([Int], Int) -> ([Int], Int)
singleIter nSteps (xs, m) = (ys ++ [m], m+1)
  where
    n = length xs
    ys = take n $ drop nSteps $ cycle xs -- optimize later


iterateSpin :: Int -> Int -> [Int]
iterateSpin nSteps nTimes = fst zs
  where
    zs = head $ drop (nTimes) $ iterate (singleIter nSteps) ([0], 1)

--------------- 14 minutes 1311 ------------------------------------------------

-- what is the value after 0 after 50000000
-- optimize now



singleIter' :: Int -> (Int, Int, Int) -> (Int, Int, Int)
singleIter' !nSteps !(pos, n, v) = (pos', (n+1), v')
  where
    pos' = (pos + nSteps) `mod` n + 1
    v' = if pos' == 1 then n
                      else v
    -- ys = take n $ drop (nSteps + 1) $ cycle xs


iterate' f x = x `seq` x : iterate' f (f x)


iterateSpin' :: Int -> Int -> (Int, Int, Int)
iterateSpin' nSteps nTimes =  zs
  where
    zs = head $ drop (nTimes) $ iterate' (singleIter' nSteps) (0, 1, (negate 1))

-- findZero = head ys
--   where
--     (Foo ys _ _) = iterateSpin' 371 50000000
--     (_:r:_) = takeWhile (/= 0) ys

--- FIXME: strictness here is crucial
-- after 1 hour, and two reboots
-- 39170601
