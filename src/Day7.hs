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
                handle <- openFile "/Users/edevi86/Downloads/day7input" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task5Input = parseLine <$> task5InputRaw

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

buildTree :: [Node] -> Tree
buildTree = undefined

findRoot :: [Node] -> [String]
findRoot xs = filter (flip S.notMember m ) ys
  where
    m =  S.fromList $ xs >>= getChildren
    ys = getName <$> xs
    getChildren (Node _ _ xs) = xs
    getName (Node name _ _) = name
