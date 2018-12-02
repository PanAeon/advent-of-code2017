{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


import Codec.Picture
import Codec.Picture.Types
-- import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V
import qualified Data.Vector as DV
import Control.Monad(void)
import Data.Ratio
-- import System.Process

k :: Integer
k = 0 --960939379918958884971672962127852754715004339660129306651505519271702802395266424689642842174350718121267153782770623355993237280874144307891325963941337723487857735749823926629715517173716995165232890538221612403238855866184013235585136048828693337902491454229288667081096184496091705183454067827731551705405381627380967602565625016981482083418783163849115590225610003652351370343874461848378737238198224849863465033159410054974700593138339226497249461751545728366702369745461014655997933798537483143786841806593422227898388722980000748404719
--

greyScaleWitness ::  Image Pixel8
greyScaleWitness  = img 106 16
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [ pixel x y | x <- reverse [0..105], y <- reverse [k..(k+16)]  ]
          pixel x y
              | tuper x  y = 1
              | otherwise = 0

modf :: Rational -> Rational -> Rational
modf a n = a - (n * (fromIntegral $ floor (a / n)))

tuper :: Integer -> Integer -> Bool
tuper x y = floor (
   (
      (fromIntegral $ floor ((y % 17))) *
        (((2 % 1  )^^(  (  ( (negate 17)*(x)) - floor ( (y % 1) `modf` 2) )  )))
      ) `modf` (2 % 1)
   ) > 0

-- exportBmpWitness :: IO ()
-- exportBmpWitness = writeBitmap "wintess.bmp" $ img 232 241
--     where img w h = array ((0,0), (w - 1, h - 1)) $ pixels w h
--           pixels w h = [((x,y), pixel x y) | y <- [0 .. h-1], x <- [0 .. w-1] ]
--           pixel x y = PixelRGBA8 128 (fromIntegral x) (fromIntegral y) 255
main :: IO ()
main = do
         writeBitmap "foo.bmp" $ (greyScaleWitness)
         -- openViewer "foo.bmp"
