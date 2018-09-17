module OOP
    (
    ) where

-- from http://www.well-typed.com/blog/2018/03/oop-in-haskell/

data Counter = Counter {
    tick    :: Counter
  , display :: Int
}

mkCounter :: Int -> Counter
mkCounter n = Counter (mkCounter (n+1)) n

twice :: Int -> Counter
twice n = (mkCounter n) {
      display = n * 2
    }

-------------

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n -  1)

fix :: (a -> a) -> a
fix f = f (fix f)

fac_body :: (Int -> Int) -> Int -> Int
fac_body _    0 = 1
fac_body self n = n * self (n-1)

fac' :: Int -> Int
fac' = fix fac_body

skim_ :: (Int -> Int) -> Int -> Int
skim_ _ 1 = 1
skim_ r n = (fac_body r n ) - 1

------------------------

mkCounter' :: (Int -> Counter) -> (Int -> Counter)
mkCounter' self n = Counter {
      tick    = self (n + 1)
    , display = n
    }

twice' :: (Int -> Counter) -> (Int -> Counter)
twice' self n = (mkCounter' self n) {
      display = n * 2
    }
