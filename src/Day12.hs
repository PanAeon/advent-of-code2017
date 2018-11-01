module Day12
    (
    ) where


import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
import           Data.List
import           Text.Parsec.Char       (alphaNum, anyChar, char, digit, satisfy, spaces, string)
import           Text.Parsec.String     (Parser)
-- import           Text.Parsec.String.Char
import           Control.Applicative
import           Control.Monad
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Text.Parsec.Combinator (between, many1, sepBy)
import           Text.Parsec.Prim       (parse, parseTest)

task12InputRaw = unsafePerformIO $ do
                handle <- openFile "input/day12" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task12Input = parseLine <$> task12InputRaw

parseLine :: String -> Node
parseLine s = case (parse nodeP "" s) of
                Left err -> error (show err)
                Right n  -> n

int :: Parser Int
int = read <$> many1 digit


data Node = Node Int [Int] deriving (Show, Eq)

nodeP :: Parser Node
nodeP = Node <$> int <* spaces <* (string "<->") <* spaces <*> (sepBy  int (char ',' <* spaces) )

{-
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5

-}

numConnected :: Int -> [Node] -> Int
numConnected x xs = length $ dfs m [x] S.empty
  where
    m = toMap xs



toMap :: [Node] -> Map Int [Int]
toMap xs = M.fromList $ ( fromNode <$> xs)

fromNode (Node x xs) = (x, xs)

dfs :: Map Int [Int] -> [Int] -> Set Int -> [Int]
dfs m [] closed = S.toList $ closed
dfs m (x:xs) closed
    | S.member x closed = dfs m xs closed
    | otherwise         = dfs m xs' closed'
  where
    closed' = S.insert x closed
    children = m M.! x
    xs' = children ++ xs

{-
A group is a collection of programs that can all communicate via pipes either directly or indirectly. The programs you identified just a moment ago are all part of the same group. Now, they would like you to determine the total number of groups.
-}
connectedComponents :: [Node] -> Int
connectedComponents xs = f open
   where
     m = toMap xs
     open = M.keys m
     f [] = 0
     f (x:xs) = 1 + f ys
       where
         ys = xs \\ (dfs m [x] S.empty)



-- maybe generic dfs ..
