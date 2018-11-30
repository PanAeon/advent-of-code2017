{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE Strict       #-}
{-# LANGUAGE StrictData   #-}
module Day23
    (runProgram, taskInput, runDuet, myInput
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
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Numeric                     as N
import           Text.Parsec.Combinator      (between, many1, sepBy)
import           Text.Parsec.Prim            (parse, parseTest, try)
-- import GHC.Prim
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Vector.Unboxed         (freeze)
import qualified Data.Vector.Unboxed         as VU
import           Debug.Trace

-- import  Control.Monad.Primitive(PrimMonad)
import           Data.Either

-- simplify, take just iterate, single var, st ... and check them separately

inputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

taskInput = parseLine <$> inputRaw  "input/day23"
myInput  = parseLine <$> inputRaw  "input/day23my"


type Param = Either Int Char
data Instruction =
            Snd Param
          | Set Char Param
          | Sub Char Param
          | Add Char Param
          | Mul Char Param
          | Mod Char Param
          | Rcv Char
          | Jgz Param Param
          | Jnz Param Param
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
            <|> (Sub <$> ((try $ cmd "sub") *> register) <*> paramP)
            <|> (Mul <$> ((try $ cmd "mul") *> register) <*> paramP)
            <|> (Mod <$> ((try $ cmd "mod") *> register) <*> paramP)
            <|> (Rcv <$> ((try $ cmd "rcv") *> register))
            <|> (Jgz <$> ((try $ cmd "jgz") *> paramP) <*> paramP)
            <|> (Jnz <$> ((try $ cmd "jnz") *> paramP) <*> paramP)

                 -- (Set <$> ( (register <*> paramP)))

----- parser took half an hour !!!!
-- I'm a bit tired

-- Vector, yeah, tired ...

update :: Int -> a -> [a] -> [a]
update i x xs = (take i xs) ++ (x: (drop (1+i) xs))


evaluate :: [Instruction] -> ([Int], Int, Int) -> ([Int], Int, Int)
evaluate !is !(regs, ip, sound) = case is !! ip of

      Set r p -> (setReg r (toVal p), ip + 1, sound+1)
      Sub r p -> (setReg r (getReg r - toVal p), ip + 1, sound+1)
      Add r p -> (setReg r (getReg r + toVal p), ip + 1, sound+1)
      Mul r p -> (setReg r (getReg r * toVal p), ip + 1, sound+1)
      Mod r p -> (setReg r (getReg r `mod` toVal p), ip + 1, sound+1)

      Jnz c l -> if toVal c /= 0
                 then (regs, ip + toVal l, sound+1)
                 else (regs, ip + 1, sound+1)
  where
    idx ch = ord ch - 97
    setReg ch x = update (idx ch) x regs
    getReg ch = regs !! (idx ch)
    toVal (Left i)   = i
    toVal (Right ch) = getReg ch


iterate' f x = x `seq` x : iterate' f (f x)

runProgram :: [Instruction] -> [([Int], Int, Int)]
runProgram is = takeWhile (\case (_, ip, _) -> ip >= 0 && ip < _N ) $ iterate' (evaluate is) (xs0, 0, 0)
  where
    _N = length is
    xs0 = 0 : take 25 (repeat 0)

---------------------


type Vec s = MVector (PrimState (ST s)) Int



idx !ch = ord ch - 97

getReg :: Vec s -> Char -> ST s (Int)
getReg xs i = VM.read xs (idx i)

setReg :: Vec s -> Char -> Int -> ST s ()
setReg regs ch x =  VM.write regs (idx ch) x

toVal _ (Left i)    = pure $! i
toVal vs (Right ch) = getReg vs ch


--
-- [0,81,81,81,81,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],29,6241)

evaluateM ::  V.Vector Instruction -> (Vec s) -> (Int, Int) ->  ST s (Int, Int)
evaluateM !is !rs (!ip, !ic)  = do
    case is V.! ip of
       Set r p -> ((toVal rs p) >>= (setReg rs r)) *> (pure $! (ip + 1, ic + 1))
       Add r p -> do
                    v <- toVal rs p
                    rv <- getReg rs r
                    setReg rs r (v + rv)
                    pure $! (ip + 1, ic + 1)
       Sub r p -> do
                    v <- toVal rs p
                    rv <- getReg rs r
                    setReg rs r (rv - v)
                    pure $! (ip + 1, ic + 1)
       Mul r p -> do
                    v <- toVal rs p
                    rv <- getReg rs r
                    setReg rs r (v * rv)
                    pure $!  (ip + 1, ic + 1)
       Mod r p -> do
                    p' <- toVal rs p
                    r' <- getReg rs r
                    setReg rs r (r' `mod` p')
                    pure $! (ip + 1, ic + 1)

       Jnz c l  -> do
                     c' <- toVal rs c
                     l' <- toVal rs l
                     if (c' /= 0)
                     then pure $!  (ip + l', ic + 1)
                     else pure $!  (ip + 1, ic + 1)


runDuet :: [Instruction]-> ((Int, Int), [Int])
runDuet xs =  ((p,c), VU.toList regs)
   where
     ((p,c),regs) = (runM  (V.fromList xs)) ( (0, 0), reg0)
     reg0 = (VU.replicate 8 (0::Int)) VU.// [(idx 'a', 1)]


iterateUntilM' :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM' !p !f !v
    | p v       = return $! v
    | otherwise = (f $! v) >>= iterateUntilM' p f


queueIsEmpty _N (p, ic) = p < 0 || p >= _N  -- || ic >= 96460375
-- 320050337 - 148387
-- 9000000   - 109387
runM:: V.Vector Instruction -> ((Int, Int), VU.Vector Int) ->  ((Int, Int), (VU.Vector Int))
runM !is ((p0, c0), !regf) = runST $ (do
                     reg <- VU.thaw regf
                     (p1, c1)   <- iterateUntilM (queueIsEmpty _N) (evaluateM is reg) (p0, c0) -- this fucker skips the update if queue is empty
                     reg' <- VU.freeze reg
                     pure  ((p1,c1), reg' ))
        where
          _N = traceShowId $ V.length is


printRegisters :: ([Int], Int, Int) -> String
printRegisters (rs, p, s) = (show rs') ++ "..., p: " ++ (show p) ++ ", s: " ++ (show s)
  where
    rs' = reverse $ dropWhile (== 0) (reverse rs)

{-
def bar() = {
    (108100).to(125100, 17).filter { b =>
      Iterator.from(2).takeWhile(_ != b).exists { d =>
        b % d == 0
      }
    }.length
  }
-}
