{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
module Yoneda.Part1() where

{-
look for https://gist.github.com/Icelandjack/aecf49b75afcfcee9ead29d27cc234d5
and https://gist.github.com/Icelandjack/02069708bc75f4284ac625cd0e2ec81f/6cb8f671d5bbc8592557e5c7a5a5c224a9663973
and https://functional.works-hub.com/learn/yo-yoneda-a2965
-}

import Data.Functor.Yoneda
import Data.Char
import Data.Kind


infixr 5
  ·
class List f where
  nil :: f a
  (·) :: a -> f a -> f a

abc :: List f => f Char
abc = 'a'·'b'·'c'·nil

instance List f => List (Yoneda f) where

  -- No need to map in the empty case
  --   fmap _cont [] = []
  nil :: Yoneda f a
  nil = Yoneda (\_cont -> nil)

  -- As we cons, we apply 'cont' to the head
  --   fmap cont (a:as) = cont(a) : fmap cont as
  (·) :: a -> Yoneda f a -> Yoneda f a
  a · Yoneda as = Yoneda $ \cont ->
    cont(a) · as cont

-- :set -XTypeApplications

ordAbc' :: List g => g Int
ordAbc' = runYoneda abc ord

runYonedaAbc :: List f => (Char -> xx) -> f xx
runYonedaAbc f = f 'a'· f 'b'· f 'c'·nil
