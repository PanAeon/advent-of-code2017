{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE Strict       #-}
{-# LANGUAGE StrictData   #-}
module Day18
    (runProgram, task18Input, runDuet
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

task18InputRaw name = unsafePerformIO $ do
                handle <- openFile name ReadMode
                contents <- hGetContents handle
                pure $ (lines contents)

task18Input = parseLine <$> task18InputRaw  "input/day18"

example = parseLine <$> task18InputRaw  "input/day18example"

ex2 = parseLine <$> ["snd 1",
                     "snd 2",
                     "snd p",
                     "rcv a",
                     "rcv b",
                     "rcv c",
                     "rcv d"]


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
    toVal (Left i)   = i
    toVal (Right ch) = getReg ch


iterate' f x = x `seq` x : iterate' f (f x)

runProgram :: [Instruction] -> [([Int], Int, Int)]
runProgram is = takeWhile (\case (_, ip, _) -> ip >= 0 && ip < _N ) $ iterate' (evaluate is) (xs0, 0, 0)
  where
    _N = length is
    xs0 = take 26 (repeat 0)

------------- 2951 ------------ took a lot ...

-- data Instructions = Instructions [Instruction] Instruction [Instruction] -- TODO: use Zipper, measure performance
type Vec s = MVector (PrimState (ST s)) Int


{-# INLINE idx #-}
idx !ch = ord ch - 97

{-# INLINE getReg #-}
getReg :: Vec s -> Char -> ST s (Int)
getReg xs i = VM.unsafeRead xs (idx i)

{-# INLINE setReg #-}
setReg :: Vec s -> Char -> Int -> ST s ()
setReg regs ch x =  VM.unsafeWrite regs (idx ch) x

-- setReg' :: Vec s -> Char -> Int -> ST s (Vec s)
-- setReg' regs ch x =  do
--                        VM.unsafeWrite regs (idx ch) x
--                        pure regs

{-# INLINE toVal #-}
toVal _ (Left i)    = pure $! i
toVal vs (Right ch) = getReg vs ch


--


evaluateM ::  V.Vector Instruction -> (Vec s) -> (Int, Int, [Int], [Int], Bool) ->  ST s (Int, Int, [Int], [Int], Bool)
evaluateM is rs (!ip, !ic, !send, !recv, !_)  = do
    -- rs <- st
    case is V.! ip of
       Snd p ->   (\v -> (ip + 1, ic + 1, v:send, recv, False)) <$> (toVal rs p)
       Set r p -> ((toVal rs p) >>= (setReg rs r)) *> (pure $! (ip + 1, ic + 1, send, recv, False))
       Add r p -> do
                    v <- toVal rs p
                    rv <- getReg rs r
                    setReg rs r (v + rv)
                    pure $! (ip + 1, ic + 1, send, recv, False)
       Mul r p -> do
                    v <- toVal rs p
                    rv <- getReg rs r
                    setReg rs r (v * rv)
                    pure $!  (ip + 1, ic + 1, send, recv, False)
       Mod r p -> do
                    p' <- toVal rs p
                    r' <- getReg rs r
                    setReg rs r (r' `mod` p')
                    pure $! (ip + 1, ic + 1, send, recv, False)
       Rcv r   -> do
                    if null recv
                    then pure $! (ip, ic, send, recv, True)
                    else do
                           setReg rs r (head recv)
                           pure $! (ip + 1, ic + 1, send, tail recv, False)
                    -- r' <- getReg rs r
                    -- if (r' /= 0)
                    -- then do
                    --        if null recv
                    --        then pure (ip, ic, send, recv, True)
                    --        else do
                    --               setReg rs r (head recv)
                    --               pure (ip + 1, ic + 1, send, tail recv, False)
                    -- else pure (ip + 1, ic + 1, send, recv, False)

       Jgz c l  -> do
                     c' <- toVal rs c
                     l' <- toVal rs l
                     if (c' > 0)
                     then pure $!  (ip + l', ic + 1, send, recv, False)
                     else pure $!  (ip + 1, ic + 1, send, recv, False)


-- runMut :: V.Vector Instruction -> ST s ([Int], Int, Int)
-- runMut is = do
--         v0 <- VM.replicate 26 0
--         setReg v0 'p' 1
--         (p1, s1, _, _) <- iterateUntilM (\case (_,ic, _, _) -> ic >= 5)(evaluateM is v0) (0,0, [], [])
--         res <- UV.freeze v0
--         pure (UV.toList res, p1, s1)--undefined --((VU.freeze v0), p1, s1)

-- 127 -- too low
f:: V.Vector Instruction -> ((Int, Int, [Int], [Int], Bool), VU.Vector Int,
              (Int, Int, [Int], [Int], Bool), VU.Vector Int, Int)
             -> ((Int, Int, [Int], [Int], Bool), VU.Vector Int,
                 (Int, Int, [Int], [Int], Bool), VU.Vector Int, Int)
f !is (z0@(!p0, !c0, !s0, !r0, !rq0), !regs0, z1@(!p1, !c1, !s1, !r1, !rq1), !regs1, !numSend) = --trace ((show regs0) ++ ", " ++ (show regs1) ++ ", " ++ (show numSend) ++
                                                                                                -- "\n" ++ (show (p0, c0, length s0, length r0, rq0)) ++ ", " ++ (show (p1, c1, length s1, length r1, rq1) )) $
   let
      ((!p0', !c0', !s0', !r0', !rq0'), reg0') = runUntilEmptyQ is  regs0 z0
      ((!p1', !c1', !s1', !r1', !rq1'), reg1') = runUntilEmptyQ is  regs1 z1
   in   ((p0', c0', [], r0' ++ (reverse s1'), rq0'), reg0', (p1', c1', [],  r1' ++ (reverse s0'), rq1'), reg1', numSend + (length s1'))

terminationCond _N !((p0, c0, s0, r0, rq0), _, (p1, c1, s1, r1, rq1), _,  _) = -- trace ((show (_N, p0, p1, rq0, rq1, length s0, length r0, length s1, length r1))) $
           {-  (c0 + c1) > _M && -}  ( rq0 && rq1 && (null s0) && (null s1) && (null r0) && (null r1)) || p0 < 0 || p0 >= _N || p1 < 0 || p1 >= _N

runDuet :: [Instruction] -> Int -> ((Int, Int, [Int], [Int], Bool), VU.Vector Int, (Int, Int, [Int], [Int], Bool), VU.Vector Int, Int)
runDuet xs _M = head $! dropWhile (not . (terminationCond _N)) $ iterate'  (f is) (z0, reg0, z0, reg1, 0)
   where
     reg0 = (VU.replicate 26 (0::Int)) VU.// [(idx 'p', 0)]
     reg1 = (VU.replicate 26 (0::Int)) VU.// [(idx 'p', 1)]
     _N = length xs
     is = (V.fromList xs)
     z0 = (0, 0, [], [], False)



     -- r0 and r1 are empty

  -- runST $ do
  --   reg0 <- VM.replicate 26 0
  --   setReg reg0 'p' 0
  --   reg0' <- VU.freeze reg0
  --   reg1 <- VM.replicate 26 0
  --   reg1' <- VU.freeze reg1
  --   setReg reg1 'p' 1
  --   let
  --     is = (V.fromList xs)
  --     z0 = (0, 0, [], [], False)
  --     _N = V.length is
  --     f !(z0@(p0, c0, s0, r0, rq0), z1@(p1, c1, s1, r1, rq1), numSend) = do
  --       ((!p0', !c0', !s0', !r0', !rq0'), reg0'') <- runUntilEmptyQ is reg0' z0
  --       ((!p1', !c1', !s1', !r1', !rq1'), reg1'') <- runUntilEmptyQ is reg1' z1
  --       pure $!  ((p0', c0', [], r0' ++ (reverse s1'), rq0'), (p1', c1', [],  r1' ++ reverse s0', rq1'), numSend + (length s0')) -- r0 and r1 are empty
  --     terminationCond !((p0, c0, s0, r0, rq0), (p1, c1, s1, r1, rq1), _) =
  --      {-  (c0 + c1) > _M && -}  ( rq0 && rq1 && (null r0) && (null r1)) || p0 < 0 || p0 >= _N || p1 < 0 || p1 >= _N
  --
  --   pure $ iterateUntilM' terminationCond f (z0, z0, 0)
    -- runUntilEmptyQ is r1 v0
    -- pure undefined



--iterate' f x = x `seq` x : iterate' f (f x)
-- {-# INLINE iterateUntilM' #-}

iterateUntilM' :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM' !p !f !v
    | p v       = return $! v
    | otherwise = (f $! v) >>= iterateUntilM' p f


queueIsEmpty _N (p, ic, send, recv, req) = p < 0 || p >= _N || (req && null recv)

runUntilEmptyQ :: V.Vector Instruction -> (VU.Vector Int) -> (Int, Int, [Int], [Int], Bool) ->  ((Int, Int, [Int], [Int], Bool), (VU.Vector Int))
runUntilEmptyQ is regf z = runST $ (do
                     reg <- VU.thaw regf
                     res   <- iterateUntilM (queueIsEmpty _N) (evaluateM is reg) z -- this fucker skips the update if queue is empty
                     reg' <- VU.freeze reg
                     pure  (res, reg' ))
        where
          _N = V.length is




-- runMutable :: [Instruction] -> ([Int], Int, Int)
-- runMutable is = runST res --takeWhile (\case (_, ip, _) -> ip >= 0 && ip < _N ) $ iterate' (evaluate is) (xs0, 0, 0)
--   where
--     _N = length is
--     res = runMut (V.fromList is)




printRegisters :: ([Int], Int, Int) -> String
printRegisters (rs, p, s) = (show rs') ++ "..., p: " ++ (show p) ++ ", s: " ++ (show s)
  where
    rs' = reverse $ dropWhile (== 0) (reverse rs)

----------
vexample :: PrimMonad m => m (MVector (PrimState m) Int)
vexample = do
  v <- VM.new 10
  forM_ [0..9] $ \i ->
     VM.write v i (2*i)
  pure v

  -- That's not the right answer; your answer is too high, 7493,
  -- curiously that's the right answer for other account
