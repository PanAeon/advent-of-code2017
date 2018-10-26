module Day10 where

input :: [Int]
input = [34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167]

test1 :: [Int]
test1 = [3, 4, 1, 5]

n = 256
-- tieAllKnots :: [Int] -> [[Int]]
tieAllKnots xs = rs
  where
    a0 = take n [0..]
    (_, rs) = foldl f ((0,0),a0) xs
    f ((skip, curr), as) l = ((skip+1, (l + skip + curr) `mod` n), rotate as curr l)
rotate xs p l = ws2 -- as ++ bs' ++ rs --take n $ drop (n - p ) $ cycle  ( (reverse xss) ++ yss)
      where
        cs = drop p (cycle xs) -- shift left by p
        rs1 = reverse (take l cs) -- reverse next l
        rs2 = drop l cs -- rest
        ws  = take n $ rs1 ++ rs2
        ws2 =  take n $ drop (n - p) $ cycle ws
        -- cs = cycle xs
        -- as = take p xs
        -- bs = take l $ drop p cs
        -- bs' = reverse bs
        -- rs  = take (n-l - (length as)) $ drop (l+p) cs
      --  xss = take l $ drop p $ cycle xs
        --yss = take (n - l) $ drop l $ drop p $ cycle xs


-- 37442 too low
