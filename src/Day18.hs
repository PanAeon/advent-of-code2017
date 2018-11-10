{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE Strict       #-}
{-# LANGUAGE StrictData   #-}
module Day18
    (runProgram, task18Input
    ) where


-- FUCK, period is 126... uhu

import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as UV
import           System.IO
import           System.IO.Unsafe


import           Data.Char
-- import           FunctionsAndTypesForParsing   (parseWithEof, parseWithLeftOver, regularParse)
import           Data.List
import           Text.Parsec.Char            (alphaNum, anyChar, char, digit, oneOf, satisfy, spaces, string)
import           Text.Parsec.String          (Parser)
-- import           Text.Parsec.String.Char
import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust)
import           Data.Set                    (Set)
import qualified Data.Set                    as S
-- import qualified Data.Vector.Algorithms.Insertion as IS
import qualified Numeric                     as N
import           Text.Parsec.Combinator      (between, many1, sepBy)
import           Text.Parsec.Prim            (parse, parseTest, try)


import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Either

task18InputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task18Input = parseLine <$> task18InputRaw  "input/day18"

example = parseLine <$> task18InputRaw  "input/day18example"

type Param = Either Int Char
data Instruction =
            Snd Param
          | Set Char Param
          | Add Char Param
          | Mul Char Param
          | Mod Char Param
          | Rcv Char
          | Jgz Param Param
           deriving (Show, Eq)

parseLine :: String -> Instruction
parseLine s = case (parse instructionP "" s) of
                Left err -> error (show err)
                Right n  -> n

-- FIXME: create module / contribute parsec-numbers ...
int :: Parser Int
int = ap sign (read <$> many1 digit)

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

register :: Parser Char
register = token (oneOf ['a'..'z'])

token :: Parser a -> Parser a
token p = p <* spaces

cmd = token . string

paramP :: Parser Param
paramP = token ((Left <$> (try int)) <|> (Right <$> register))

-- it was faster to add those puny instructions by hand
instructionP :: Parser Instruction
instructionP =  (Snd <$> ((try $ cmd "snd") *> paramP))
            <|> (Set <$> ((try $ cmd "set") *> register) <*> paramP)
            <|> (Add <$> ((try $ cmd "add") *> register) <*> paramP)
            <|> (Mul <$> ((try $ cmd "mul") *> register) <*> paramP)
            <|> (Mod <$> ((try $ cmd "mod") *> register) <*> paramP)
            <|> (Rcv <$> ((try $ cmd "rcv") *> register))
            <|> (Jgz <$> ((try $ cmd "jgz") *> paramP) <*> paramP)

                 -- (Set <$> ( (register <*> paramP)))

----- parser took half an hour !!!!
-- I'm a bit tired

-- Vector, yeah, tired ...

update :: Int -> a -> [a] -> [a]
update i x xs = (take i xs) ++ (x: (drop (1+i) xs))


evaluate :: [Instruction] -> ([Int], Int, Int) -> ([Int], Int, Int)
evaluate !is !(regs, ip, sound) = case is !! ip of
      Snd p -> (regs, ip + 1, (toVal p))
      Set r p -> (setReg r (toVal p), ip + 1, sound)
      Add r p -> (setReg r (getReg r + toVal p), ip + 1, sound)
      Mul r p -> (setReg r (getReg r * toVal p), ip + 1, sound)
      Mod r p -> (setReg r (getReg r `mod` toVal p), ip + 1, sound)
      Rcv r -> if getReg r /= 0
               then error ("Wow: " ++ show sound ) --(setReg r sound, ip + 1, sound)
               else (regs, ip + 1, sound)
      Jgz c l -> if toVal c > 0
                 then (regs, ip + toVal l, sound)
                 else (regs, ip + 1, sound)
  where
    idx ch = ord ch - 97
    setReg ch x = update (idx ch) x regs
    getReg ch = regs !! (idx ch)
    toVal (Left i) = i
    toVal (Right ch) = getReg ch


iterate' f x = x `seq` x : iterate' f (f x)

runProgram :: [Instruction] -> [([Int], Int, Int)]
runProgram is = takeWhile (\case (_, ip, _) -> ip >= 0 && ip < _N ) $ iterate' (evaluate is) (xs0, 0, 0)
  where
    _N = length is
    xs0 = take 256 (repeat 0)


------------- 2951 ------------ took a lot ...

-- it's meant to be run twice at the same time.

{-

    snd X sends the value of X to the other program. These values wait in a queue until that program is ready to receive them. Each program has its own message queue, so a program can never receive a message it sent.
    rcv X receives the next value and stores it in register X. If no values are in the queue, the program waits for a value to be sent to it. Programs do not continue to the next instruction until they have received a value. Values are received in the order they are sent.

-}

---------------------------------------------------------------------
