module UnderstandingContinuations
    (
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
-- from https://www.schoolofhaskell.com/user/jwiegley/understanding-continuations


main' = putStrLn "alpha" -- main
       >>= \_ ->            -- k
           putStrLn "beta"
           >>= \_ ->        -- j
               putStrLn "gamma"

main'' = flip runContT return $ do
    lift $ putStrLn "alpha"
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j


main''' = flip runContT return $ do
    lift $ putStrLn "alpha"
    callCC $ \k -> do
      k ()
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j

main'''' = flip runContT return $ do
    lift $ putStrLn "alpha"
    callCC $ \k -> do
      k ()
      lift $ putStrLn "uh oh..."
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j



main5 = flip runContT return $ do
    lift $ putStrLn "alpha"
    num <- callCC $ \k -> do
      if 42 == 7 * 6
          then k 42
          else lift $ putStrLn "uh oh..."
      return 43
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    lift $ print num                -- l


main6 = flip runContT return $ do
    lift $ putStrLn "alpha"
    (k, num) <- callCC $ \k -> let f x = k (f, x)
                               in return (f, 0)
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    if num < 5
        then k (num + 1) >> return ()
        else lift $ print num       -- l
