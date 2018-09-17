module Day3
    (
    ) where
import           Data.List
import           Data.Map.Strict as M
import           Debug.Trace

spiral :: Int -> Int
spiral 1 = 0
spiral n = trace ("k: " ++ (show k) ++ ", c:" ++ (show c) ++ ", s: " ++ (show s) ++ ", pw: " ++ (show pw) ++ ", w: " ++ (show w) ++ ", n'" ++ show n' ++ ", d':" ++ show d') $ hw + d'
  where
    s = sqrt (fromIntegral n)
    c = ceiling s
    w = if even c then c + 1 else c
    pw = w - 2
    hw = w `div` 2
    (k, n') = (n - pw * pw) `divMod` (hw)
    d' = if odd k then n' else (hw - n')-- abs

toCoords :: Int -> (Int, Int)
toCoords 1 =  (0,0)
toCoords n =  (qx + x, qy + y)
  where
    c = ceiling $ sqrt (fromIntegral n)
    w = if even c then c + 1 else c
    pw = w - 2
    hw = w `div` 2
    (k, n') = (n - pw * pw) `divMod` (hw) -- 2 is a special case :(
    xs = [(hw*dx, hw*dy) | (dx, dy) <- [(0,1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1), (1,0), (1,1)]]
    ys = [(n'*dx, n'*dy) | (dx, dy) <- [(-1,0), (0, -1), (0, -1), (1, 0), (1, 0), (0, 1), (0,1), (-1,0)]]
    (qx, qy) = xs !! (k - 1)
    (x, y) = ys !! (k - 1) -- toCoords 10 ? // negative index ..
{-
17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...
-}
-- 371 ? on 368078

-- spiral 4 -- c:2, s: 2.0, pw: 1, w: 3, n'0, d':1

--------------------------------------------------------------
-- part II
--------------------------------------------------------------


{-
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
-}

-- not super efficient, but easy to grasp
generateSpiral :: Int -> ((Int, Int), Int) -> [((Int, Int), Int)]
generateSpiral 1 _ = [((0, 0), 1)] ++ generateSpiral 3 ((0, 0), 1)
generateSpiral width ((px, py), pv) = current ++ generateSpiral (width + 2) (last current)
  where
    current = genSegment width (pv+1) (px+1, py)

  --  []

printSpiral :: Int -> String
printSpiral n = intercalate "\n" ys
  where
    hw = n `div` 2
    s = take ((n)*(n)) $ generateSpiral 1 ((0,0),1)
    m1  = M.fromList s
    ys = do
           dy <- reverse [(negate hw)..hw]
           let row = do
                      dx <- [(negate hw)..hw]
                      let pv =  m1 M.! (dx, dy)
                      pure (show pv)
               r = intercalate "\t" row
           pure r





genSegment :: Int -> Int -> (Int, Int) -> [((Int, Int), Int)]
genSegment width n (x,y) = s1 ++ s2 ++ s3 ++ s4
  where
    hw = (width `div` 2)

    (s1, (x1,y1), n1) = goUp n (width-1) (x,y)
    (s2,  (x2,y2), n2) = goLeft n1 (width-1) (x1-1,y1)
    (s3,  (x3,y3), n3) = goDown n2 (width-1) (x2,y2-1)
    (s4, _, _) = goRight n3 (width-1) (x3+1,y3)


goUp :: Int -> Int -> (Int, Int) -> ([((Int, Int), Int)], (Int, Int), Int)
goUp n m (x,y) =
  ([((x, y + i),n+i) | i <- [0..m-1]], (x, y+m-1), n+m)

goLeft :: Int -> Int -> (Int, Int) -> ([((Int, Int), Int)], (Int, Int), Int)
goLeft n m (x,y) =
  ([((x-i, y),n+i) | i <- [0..m-1]], (x-m+1, y), n+m)

goDown :: Int -> Int -> (Int, Int) -> ([((Int, Int), Int)], (Int, Int), Int)
goDown n m (x,y) =
  ([((x, y - i),n+i) | i <- [0..m-1]], (x, y-m+1), n+m)

goRight :: Int -> Int -> (Int, Int) -> ([((Int, Int), Int)], (Int, Int), Int)
goRight n m (x,y) =
  ([((x+i, y),n+i) | i <- [0..m-1]], (x+m-1, y), n+m)
