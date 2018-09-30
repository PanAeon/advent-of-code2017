{-# LANGUAGE LambdaCase #-}
module Day7
    (
    ) where

import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
import           Text.Parsec.Char       (alphaNum, anyChar, char, digit, spaces, string)
import           Text.Parsec.String     (Parser)
-- import           Text.Parsec.String.Char
import           Control.Applicative
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Text.Parsec.Combinator (between, many1, sepBy)
import           Text.Parsec.Prim       (parse, parseTest)

task5InputRaw = unsafePerformIO $ do
                handle <- openFile "input/day7" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task5Input = parseLine <$> task5InputRaw

rootNode = Node "bpvhwhh" 60 ["lzfgxlb","fzclaow","kfdxxb","xnmjpa","rilgrr","fvrrpo","zcmlgn"]
myTree = buildTree task5Input rootNode

data Node = Node String Int [String] deriving Show

data Tree = TNode String Int [Tree] deriving Show


num :: Parser Int
num = read <$> (many1 digit)


str :: Parser String
str = many1 alphaNum

nodeParser :: Parser Node
nodeParser = Node <$> (str <* spaces)
                  <*> (between (char '(')  (char ')') num)
                  <* optional (string " -> ")
                  <*> ((spaces *> str) `sepBy` (char ','))

parseLine :: String -> Node
parseLine s = case (parse nodeParser "" s) of
                Left err -> error (show err)
                Right n  -> n

buildTree :: [Node] -> Node -> Tree
buildTree _       (Node id' weight []) = TNode id' weight []
buildTree mapping (Node id' weight xs) = TNode id' weight ys
  where
    getNode name = head $ filter ( (== name) . getName ) mapping
    ys = buildTree mapping . getNode <$> xs



findRoot :: [Node] -> [String]
findRoot xs = filter (flip S.notMember m ) ys
  where
    m =  S.fromList $ xs >>= getChildren
    ys = getName <$> xs


getChildren (Node _ _ xs) = xs
getName (Node name _ _) = name

balance :: Tree -> (String, Int, [[(String, Int)]])
balance (TNode id' w []) = (id', w, [])
balance (TNode id' w children) = if isBalanced
                                 then (id', (w + sum weights), unbalancedBefore)
                                 else (id', (w + sum weights), unbalancedNow : unbalancedBefore)
  where
    zs = balance <$> children
    unbalancedBefore =  zs >>= (\case (_, _, xs) -> xs)
    weights = (\case (_, w, _) -> w) <$> zs
    s1 =  head weights
    isBalanced = all (== s1) weights
    unbalancedNow =  (\case (_i, _w, _) -> (_i, _w))<$> zs
