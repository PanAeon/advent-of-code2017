module Day8
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
import           Control.Monad
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Text.Parsec.Combinator (between, many1, sepBy)
import           Text.Parsec.Prim       (parse, parseTest)
-- import qualified Text.ParserCombinators.Parsec.Number as PN

task8InputRaw = unsafePerformIO $ do
                handle <- openFile "input/day8" ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task8Input = parseLine <$> task8InputRaw

parseLine :: String -> Instruction
parseLine s = case (parse instructionP "" s) of
                Left err -> error (show err)
                Right n  -> n

int :: Parser Int
int = ap sign (read <$> many1 digit)

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)


regNameP :: Parser String
regNameP = many1 alphaNum

data ConditionOp = LT' | GT' | LE' | GE' | EQ' | NE' deriving Show
data Instruction = Instruction String Bool Int String ConditionOp Int

instance Show Instruction where
  show (Instruction reg incr amount creg cop camount) =
      reg ++ " " ++ action ++ " " ++ (show amount) ++ " if " ++ creg ++ " "
       ++ (show cop) ++ " " ++ (show camount)
    where
      action = if incr then "inc" else "dec"

actionP :: Parser Bool
actionP = (True <$ string "inc") <|> (False <$ string "dec")

conditionP :: Parser ConditionOp
conditionP = lessP <|> gtP
                   <|> (EQ' <$ string "==")
                   <|> (NE' <$ string "!=")
  where
     lessP = do -- FIXME: rewrite this shame
               _ <- char '<'
               e <- optional (char '=')
               pure $ if isJust e then  LE' else LT'
     gtP = do
               _ <- char '>'
               e <- optional (char '=')
               pure $ if isJust e then  GE' else GT'
         --     (LE' <$ string "<=")
         -- <|> (LT' <$ string "<")
         -- <|> (GT' <$ string ">")
         -- <|> (GE' <$ string ">=")

maxValue = maximum $ M.elems $ snd (runCode task8Input)
maxValueAllTime =  fst (runCode task8Input)

instructionP :: Parser Instruction
instructionP = Instruction <$> regNameP <* spaces <*> actionP <* spaces <*> int <* spaces <*
                               (string "if") <* spaces <*> regNameP <* spaces <*> conditionP
                               <* spaces <*> int

--[name] [inc|dec] [singed int] `if` `reg_name` [>=|<=| > | < | ==] [signed int]

runCode :: [Instruction] -> (Int, Map String Int)
runCode = foldl applyAction (0, M.empty)

applyAction :: (Int, Map String Int) -> Instruction -> (Int, Map String Int)
applyAction (pm, m) (Instruction reg incr amount creg cop camount) =
  if verifyCondition cRegValue camount cop
  then (pm', M.insert reg x' m)
  else (pm, m)
  where
    action = if (incr) then amount else negate amount
    get name = maybe 0 id $ M.lookup name m
    cRegValue = get creg
    x' = (get reg) + action
    pm' = max pm x'

verifyCondition :: Int -> Int -> ConditionOp -> Bool
verifyCondition a b LT' = a < b
verifyCondition a b GT' = a > b
verifyCondition a b LE' = a <= b
verifyCondition a b GE' = a >= b
verifyCondition a b EQ' = a == b
verifyCondition a b NE' = a /= b

{-
To be safe, the CPU also needs to know the highest value held in any register during this process
so that it can decide how much memory to allocate to these operations.
For example, in the above instructions, the highest value ever held was 10
(in register c after the third instruction was evaluated).
-}
