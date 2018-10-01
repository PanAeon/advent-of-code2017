module Day9
    (
    ) where

import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
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
-- import qualified Text.ParserCombinators.Parsec.Number as PN

task9InputRaw = head $ unsafePerformIO $ do
                handle <- openFile "input/day9" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

groups = parseLine task9InputRaw

data Group = Group [Group] deriving Show

groupP :: Parser Group
groupP = (Group . join) <$> (char '{' *> (sepBy ( ( pure <$> groupP) <|> ( [] <$ garbageP)) (char ',')) <* char '}')

garbageP :: Parser ()
garbageP = void $ between (char '<') (char '>') (many escapedString)

escapedString :: Parser ()
escapedString = void $ ( (char '!') *> anyChar) <|> ( (satisfy (/= '>')))


parseLine :: String -> Group
parseLine s = case (parse groupP "" s) of
                Left err -> error (show err)
                Right n  -> n


countGroups :: Int -> Group -> Int
countGroups n (Group xs) = (n+1) + (sum $ (countGroups (n+1)) <$> xs)

{-
so the rules are :

group := '{' sepBy ',' group | garbage '}'
garbage := '<' escapedChar* '>'
escapedChar := anyCharExcept '>' | '!' FollowedByAnyCharAtAll

-}




int :: Parser Int
int = ap sign (read <$> many1 digit)

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)


regNameP :: Parser String
regNameP = many1 alphaNum


{-

--- Part Two ---

Now, you're ready to remove the garbage.

To prove you've removed it, you need to count all of the characters within the garbage.
The leading and trailing < and > don't count, nor do any canceled characters or the ! doing the canceling.

    <>, 0 characters.
    <random characters>, 17 characters.
    <<<<>, 3 characters.
    <{!>}>, 2 characters.
    <!!>, 0 characters.
    <!!!>>, 0 characters.
    <{o"i!a,<{i<a>, 10 characters.

How many non-canceled characters are within the garbage in your puzzle input?


-}

-- actionP :: Parser Bool
-- actionP = (True <$ string "inc") <|> (False <$ string "dec")
--
-- conditionP :: Parser ConditionOp
-- conditionP = lessP <|> gtP
--                    <|> (EQ' <$ string "==")
--                    <|> (NE' <$ string "!=")
--   where
--      lessP = do -- FIXME: rewrite this shame
--                _ <- char '<'
--                e <- optional (char '=')
--                pure $ if isJust e then  LE' else LT'
--      gtP = do
--                _ <- char '>'
--                e <- optional (char '=')
--                pure $ if isJust e then  GE' else GT'
--          --     (LE' <$ string "<=")
--          -- <|> (LT' <$ string "<")
--          -- <|> (GT' <$ string ">")
--          -- <|> (GE' <$ string ">=")



-- instructionP :: Parser Instruction
-- instructionP = Instruction <$> regNameP <* spaces <*> actionP <* spaces <*> int <* spaces <*
--                                (string "if") <* spaces <*> regNameP <* spaces <*> conditionP
--                                <* spaces <*> int
