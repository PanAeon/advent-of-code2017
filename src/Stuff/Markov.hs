{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Stuff.Markov where


-- Main takeaways:
-- 1. Mathematics can't code
-- 2. Coupling from the past
-- ( coupling from the past gives in principle a perfect sample from the stationary distribution)
-- 3. ITERATED RaNdom Functions
-- https://www.stat.berkeley.edu/~aldous/205B/511.pdf

-- SRC: http://blog.sigfpe.com/2018/10/running-from-past.html



import           Control.Monad
import           Control.Monad.State
import           Data.Array
import           Data.List
import           Data.Sequence       (replicateA)
import           System.Random

data ABC = A | B | C deriving (Eq, Show, Ord, Enum, Bounded)

uniform :: (RandomGen gen, MonadState gen m) => m Double
uniform = state random

step :: (RandomGen gen, MonadState gen m) => ABC -> m ABC
step A = do
    a <- uniform
    if a < 0.5
        then return A
        else return B
step B = do
    a <- uniform
    if a < 1/3.0
        then return A
        else if a < 2/3.0
            then return B
            else return C
step C = do
    a <- uniform
    if a < 0.5
        then return B
        else return C

steps :: (RandomGen gen, MonadState gen m) => Int -> ABC -> m ABC
steps n x = foldM (const . step) x [0..n-1]

steps' :: (RandomGen gen, MonadState gen m) => Int -> m (ABC -> ABC)
steps' n = (foldr (flip (.)) id) <$> (replicateA n step')


steps_from_past :: (RandomGen gen, MonadState gen m) => Int -> m (ABC -> ABC)
steps_from_past n = (foldr ((.)) id) <$> (replicateA n step')
  -- do
  -- fs <- replicateA n step'
  -- return $ foldr (flip (.)) id fs

gen = mkStdGen 2

step' :: (RandomGen gen, MonadState gen m) => m (ABC -> ABC)
step' = do
    a <- uniform
    return $ \case
        A -> if a < 0.5 then A else B
        B -> if a < 1/3.0
                then A
                else if a < 2/3.0 then B else C
        C -> if a < 0.5 then B else C
