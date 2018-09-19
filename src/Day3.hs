module Day3
    (
    ) where
import           Control.Arrow
import           Data.List
import qualified Data.Map.Strict as M
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

-- toCoords :: Int -> (Int, Int)
-- toCoords 1 =  (0,0)
-- toCoords n =  (qx + x, qy + y)
--   where
--     c = ceiling $ sqrt (fromIntegral n)
--     w = if even c then c + 1 else c
--     pw = w - 2
--     hw = w `div` 2
--     (k, n') = (n - pw * pw) `divMod` (hw) -- 2 is a special case :(
--     xs = [(hw*dx, hw*dy) | (dx, dy) <- [(0,1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1), (1,0), (1,1)]]
--     ys = [(n'*dx, n'*dy) | (dx, dy) <- [(-1,0), (0, -1), (0, -1), (1, 0), (1, 0), (0, 1), (0,1), (-1,0)]]
--     (qx, qy) = xs !! (k - 1)
--     (x, y) = ys !! (k - 1) -- toCoords 10 ? // negative index ..
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
-- generateSpiral :: Int -> ((Int, Int), Int) -> [((Int, Int), Int)]
-- generateSpiral 1 _ = [((0, 0), 1)] ++ generateSpiral 3 ((0, 0), 1)
-- generateSpiral width ((px, py), pv) = current ++ generateSpiral (width + 2) (last current)
--   where
--     (current, next) = genSegment width (pv+1) (px+1, py)

  --  []

type Elem =  ((Int, Int), Int)


printSpiral :: Int -> String
printSpiral n = intercalate "\n" ys
  where
    hw = n `div` 2

    s =  concatMap (id) $ take n $ snd <$> iterate sumSpiralStep (3, [((0,0),1)]) --take ((n)*(n)) $
    m1  = M.fromList s
    ys = do
           dy <- reverse [(negate hw)..hw]
           let row = do
                      dx <- [(negate hw)..hw]
                      let pv =  m1 M.! (dx, dy)
                      pure (show pv)
               r = intercalate "\t" row
           pure r

result = head $ dropWhile ((<= 368078) . snd) $ (concatMap (id)) $ snd <$> iterate sumSpiralStep (3, [((0,0),1)])
--
sumSpiralStep :: (Int, [Elem]) -> (Int, [Elem]) -- returns latest
sumSpiralStep (w,prev) = (w+2, current)
  where
    ((dx, dy), pv) = last prev
    (current, _) = genSegment (w,prev) (pv+1) (dx+1, dy)


genSegment :: (Int, [Elem]) -> Int -> (Int, Int) -> ([Elem], Elem)
genSegment (width, prev) n (x,y) = (as ++ bs ++ cs ++ ds, d)
  where
    -- hw = (width `div` 2)
    prevM = M.fromList prev
    (as, a) = go up' width prevM (width-2) ((x,y),n) []
    (bs,b) = go left' width prevM (width-1) (a) as
    (cs, c) = go down' width prevM (width-1) b (as ++ bs)
    (ds, d) = go right' width prevM (width) c (as ++ bs ++ cs)



boxCoords (x,y) = [(x + dx, y + dy)| dx <- [-1..1], dy <- [-1..1]]

safeHead :: [a] -> a -> a
safeHead [] x    = x
safeHead (x:_) _ = x

go :: (Int -> (Int, Int) -> (Int, Int)) -> Int -> M.Map (Int, Int) Int -> Int -> ((Int, Int), Int) -> [Elem] -> ([((Int, Int), Int)], ((Int, Int), Int))
go f width prevM n (x,m) ps =  (xs, (f 1 x', m'+1))
  where
    prevHW = (width - 2) `div` 2
    hw = width `div` 2
    insidePrev (x,y) = x <= prevHW && x >= (negate prevHW) &&
                     y <= prevHW && y >= (negate prevHW)
    insideBorder (x,y) = x == hw || x == (negate hw) ||
                         y == hw || y == (negate hw)
    psMap = M.fromList ps

    getPrevValues (x,y) =  sum $ (prevM M.!) <$> (filter insidePrev $ boxCoords (x,y))
    getLastS (x,y) = sum $ (\x -> M.findWithDefault 0 x psMap) <$> (filter insideBorder $ boxCoords (x,y))

    g []     i = (f i x, getLastS (f i x) + getPrevValues (f i x)):[]
    -- g (a:[]) i = (f i x, getLastS (f i x) + (snd a ) + getPrevValues (f i x)):a:[]
    g (a:as) i = (f i x, getLastS (f i x) + (snd a ) + getPrevValues (f i x)):a:as
    xs = reverse $ foldl g [] [0..n-1]
    xs' = [(f i x, getPrevValues (f i x)) | i <- [0..n-1]]
    (x',m') = last xs

up' :: Int -> (Int, Int) -> (Int, Int)
up' i  = second (+i)

down' :: Int -> (Int, Int) -> (Int, Int)
down' i  = second (subtract i)

left' :: Int -> (Int, Int) -> (Int, Int)
left' i  = first (subtract i)

right' :: Int -> (Int, Int) -> (Int, Int)
right' i  = first (+i)
